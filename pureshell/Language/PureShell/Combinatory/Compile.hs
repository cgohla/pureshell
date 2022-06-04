{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeApplications   #-}
module Language.PureShell.Combinatory.Compile where

import qualified Language.PureShell.Combinatory.Types as C
import qualified Language.PureShell.Identifiers       as Ids
import qualified Language.PureShell.Procedural        as P

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8 (pack)
import           Data.List.Extra                      (snoc)
import           Data.Singletons
import           Data.String                          (fromString)
import           Polysemy                             (Member, Sem, run)
import           Polysemy.Writer                      (Writer, runWriter, tell)

type TopLevelFunDefs = [P.FunDef ByteString]

-- | Presumably the main entry point in this module
lowerModule :: C.Module (ss :: [C.Foo]) -> P.Module ByteString
lowerModule = C.moduleFold f
  where
    f n b = n <> (P.Module $ lowerOneTopLevelDefn b)

lowerOneTopLevelDefn :: forall s. C.TopLevelBind (s :: C.Foo) -> TopLevelFunDefs
lowerOneTopLevelDefn = uncurry snoc . run . runWriter @TopLevelFunDefs . Ids.runLocalNames @Ids.SimpleBashFunName . lowerTopLevelBind

lowerTopLevelBind :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                     , Member (Writer TopLevelFunDefs) r )
                  => C.TopLevelBind s -> Sem r (P.FunDef ByteString)
lowerTopLevelBind (C.Bind i e) = do
  fn <- Ids.mkName @Ids.SimpleBashFunName $ fromString $ show $ fromSing i
  let lowerExpr' = Ids.runLocalNames @Ids.LocalBashVarName . lowerExpr
  case e of
    C.Abs c f -> P.FunDef fn ps <$> lowerExpr' f
      where
        ps = fmap mkParamName $ C.unContext $ fromSing c
        mkParamName :: C.Foo -> Ids.LocalBashVarName -- apparently the sig is required
        mkParamName = Ids.LocalBashVarName . C8.pack . show
    _ -> P.FunDef fn [] <$> t
      where
        t = lowerExpr' e

lowerExprLiteral :: (Member (Writer TopLevelFunDefs) r)
             => C.Literal c -> Sem r (P.Sequence ByteString)
lowerExprLiteral = \case
  C.StringLiteral s         -> pure $ literal s
  C.NumericLiteral (Left n) -> pure $ literal $ show n
  _                         -> error "not implemented"
  where
    literal = P.Sequence [] . P.Literal . C8.pack

lowerExprApp :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Writer TopLevelFunDefs) r)
             => C.Expr c -> C.ExprList d -> Sem r (P.Sequence ByteString)
lowerExprApp e es = do
  (v, a) <- exprEvalAssign e -- TODO this produces wrong results in the case of Prim
  (vs, as) <- C.genExprListFold chainExprEval es -- TODO if an es is a Var then we should use it directly
  let b = P.Application (P.ClosureFromVar v) vs -- TODO if e is a Prim we may want to use the literal name
  pure $ P.Sequence (a:as) b

exprEvalAssign :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                  , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                  , Member (Writer TopLevelFunDefs) r)
               => C.Expr c -> Sem r (Ids.LocalBashVarName, P.Assignment ByteString)
exprEvalAssign e = do
  s <- lowerExpr e
  v <- Ids.mkName @Ids.LocalBashVarName "r"
  pure $ (v, P.Assignment v s)

chainExprEval :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                 , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                 , Member (Writer TopLevelFunDefs) r)
              => Sem r ([Ids.LocalBashVarName], [P.Assignment ByteString])
              -> C.Expr c -> Sem r ([Ids.LocalBashVarName], [P.Assignment ByteString])
chainExprEval as e = do
  (vs, bs) <- as
  (v, a) <-  exprEvalAssign e
  pure $ (vs <> [v], bs <> [a])

-- lowerExprPrim :: String -> Sem r (P.Sequence ByteString)
-- lowerExprPrim n = pure $ P.Sequence [] $ P.Application (P.ClosureFromName $ Ids.SimpleBashFunName n') []
--   where
--     n' = C8.pack n -- This is very wrong
lowerExprPrim :: String -> Sem r (P.Sequence ByteString)
lowerExprPrim n = pure $ P.Sequence [] $ P.Literal n'
  where
    n' = C8.pack n -- This is very wrong

-- TODO this change doesn't solve the problem. We need a Prim value in
-- Procedural as well.

-- TODO It might be worthwhile to decouple the type for value literals
-- and parametrize Combinatory and Procedural over it. (This is a
-- separate issue from the above).

lowerExprAbs :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Writer TopLevelFunDefs) r)
             => Sing (c :: C.Context) -> C.Expr d -> Sem r (P.Sequence ByteString)
             -- NOTE we are not actually enforcing the binding
             -- constraint here anymore. maybe there is an easy way to
             -- do that
lowerExprAbs c e = Ids.runLocalNames @Ids.LocalBashVarName $ do
  -- TODO this might be bad, i.e., running a concrete implementation
  -- in business code. Use 'bracket' and 'resources' here.
  n <- Ids.mkName @Ids.SimpleBashFunName "lambda"
  -- TODO generalize, so we can include a better name
  let mkName' = Ids.mkName @Ids.LocalBashVarName . Ids.LocalBashVarName . C8.pack . show
  -- TODO this should guarantee we are starting a new local scope
  vs <- traverse mkName' $ C.unContext $ fromSing c
  s <- lowerExpr e
  let f = P.FunDef n vs s
  tell @TopLevelFunDefs [f]
  let a = P.Application (P.ClosureFromName n) [] -- TODO the empty list seems wrong
  pure $ P.Sequence [] a

lowerExprLet :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Ids.LocalNames Ids.LocalBashVarName) r
                , Member (Writer TopLevelFunDefs) r)
             => C.Bind s c -> C.Expr d -> Sem r (P.Sequence ByteString)
lowerExprLet (C.Bind i e) f = do
  let mkName' = Ids.mkName @Ids.LocalBashVarName . Ids.LocalBashVarName . C8.pack . show
  n <- mkName' $ fromSing i
  b <- P.Assignment n <$> lowerExpr e
  P.Sequence s a <- lowerExpr f
  -- TODO this is not quite right. f needs to be able to capture
  -- i. but if i is already taken, we will get a different name back,
  -- and the capute would fail.
  --
  -- the question seems to be how to shadow correctly. we could use a
  -- naming effect to make sure that variable references in f are
  -- resolved correctly.
  pure $ P.Sequence (b:s) a
  -- TODO implement a renaming effect


lowerExprVar :: Sing (s :: C.Foo) -> Sem r (P.Sequence ByteString)
lowerExprVar n  = pure $ P.Sequence [] $ P.Variable $ Ids.LocalBashVarName $ C8.pack $ show $ fromSing n

lowerExpr :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
             , Member (Ids.LocalNames Ids.SimpleBashFunName) r
             , Member (Writer TopLevelFunDefs) r)
          => C.Expr c -> Sem r (P.Sequence ByteString)
lowerExpr = \case
  C.Var n    -> lowerExprVar n
  C.Lit l    -> lowerExprLiteral l
  C.App e es -> lowerExprApp e es
  C.Abs c e  -> lowerExprAbs c e
  C.Prim n   -> lowerExprPrim n
  C.Let b e  -> lowerExprLet b e
