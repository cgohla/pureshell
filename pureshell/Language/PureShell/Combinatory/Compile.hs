{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}

module Language.PureShell.Combinatory.Compile where

import qualified Language.PureShell.Combinatory.Types as Combinatory
import qualified Language.PureShell.Identifiers       as Ids
import qualified Language.PureShell.Procedural        as Procedural

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8 (pack)
import           Data.Singletons
import           Data.String                          (fromString)
import           Polysemy                             (Member, Sem)
import           Polysemy.Writer                      (Writer, tell)

-- lowerToProcedural :: Combinatory.Module -> Procedural.Module ByteString
-- lowerToProcedural m = undefined

type TopLevelFunDefs = [Procedural.FunDef ByteString]

lowerTopLevelBind :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                     , Member (Writer TopLevelFunDefs) r )
                  => Combinatory.TopLevelBind -> Sem r (Procedural.FunDef ByteString)
lowerTopLevelBind (Combinatory.Bind i e) = do
  fn <- Ids.mkName @Ids.SimpleBashFunName $ fromString $ show i
  t <- Ids.runLocalNames @Ids.LocalBashVarName $ lowerExpr e
  let ps = case e of
        Combinatory.Abs _ _ -> error "recovering params not implemented"
        _                   -> []
  pure $ Procedural.FunDef fn ps t

lowerExprLiteral :: (Member (Writer TopLevelFunDefs) r)
             => Combinatory.Literal c -> Sem r (Procedural.Sequence ByteString)
lowerExprLiteral = \case
  Combinatory.StringLiteral s         -> pure $ literal s
  Combinatory.NumericLiteral (Left n) -> pure $ literal $ show n
  _                                   -> error "not implemented"
  where
    literal = Procedural.Sequence [] . Procedural.Literal . C8.pack

lowerExprApp :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Writer TopLevelFunDefs) r)
             => Combinatory.Expr c -> Combinatory.ExprList d -> Sem r (Procedural.Sequence ByteString)
lowerExprApp e es = do
  (v, a) <- exprEvalAssign e
  (vs, as) <- Combinatory.genExprListFold chainExprEval es
  let b = Procedural.Application (Procedural.ClosureFromVar v) vs -- TODO if e is a Prim we may want to use the literal name
  pure $ Procedural.Sequence (a:as) b

exprEvalAssign :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                  , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                  , Member (Writer TopLevelFunDefs) r)
               => Combinatory.Expr c -> Sem r (Ids.LocalBashVarName, Procedural.Assignment ByteString)
exprEvalAssign e = do
  s <- lowerExpr e
  v <- Ids.mkName @Ids.LocalBashVarName "r"
  pure $ (v, Procedural.Assignment v s)

chainExprEval :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                 , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                 , Member (Writer TopLevelFunDefs) r)
              => Sem r ([Ids.LocalBashVarName], [Procedural.Assignment ByteString])
              -> Combinatory.Expr c -> Sem r ([Ids.LocalBashVarName], [Procedural.Assignment ByteString])
chainExprEval as e = do
  (vs, bs) <- as
  (v, a) <-  exprEvalAssign e
  pure $ (vs <> [v], bs <> [a])

lowerExprPrim :: String -> Sem r (Procedural.Sequence ByteString)
lowerExprPrim n = pure $ Procedural.Sequence [] $ Procedural.Application (Procedural.ClosureFromName $ Ids.SimpleBashFunName n') []
  where
    n' = C8.pack n -- This is very wrong

lowerExprAbs :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Writer TopLevelFunDefs) r)
             => Sing (c :: Combinatory.Context) -> Combinatory.Expr d -> Sem r (Procedural.Sequence ByteString)
             -- NOTE we are not actually enforcing the binding
             -- constraint here anymore. maybe there is an easy way to
             -- do that
lowerExprAbs c e = Ids.runLocalNames @Ids.LocalBashVarName $ do
  n <- Ids.mkName @Ids.SimpleBashFunName "lambda"
  let mkName' = Ids.mkName @Ids.LocalBashVarName . Ids.LocalBashVarName . C8.pack . show
  -- TODO this should guarantee we are starting a new local scope
  vs <- traverse mkName' $ Combinatory.unContext $ fromSing c
  s <- lowerExpr e
  let f = Procedural.FunDef n vs s
  tell @TopLevelFunDefs [f]
  let a = Procedural.Application (Procedural.ClosureFromName n) []
  pure $ Procedural.Sequence [] a

lowerExpr :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
             , Member (Ids.LocalNames Ids.SimpleBashFunName) r
             , Member (Writer TopLevelFunDefs) r)
          => Combinatory.Expr c -> Sem r (Procedural.Sequence ByteString)
lowerExpr = \case
  Combinatory.Var _    -> error "needs vars in procedural"
  Combinatory.Lit l    -> lowerExprLiteral l
  Combinatory.App e es -> lowerExprApp e es
  Combinatory.Abs c e  -> lowerExprAbs c e
  Combinatory.Prim n   -> lowerExprPrim n
