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

import qualified Language.PureShell.Combinatory.Types as Combinatory
import qualified Language.PureShell.Identifiers       as Ids
import qualified Language.PureShell.Procedural        as Procedural

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as C8 (pack)
import           Data.List.Extra                      (snoc)
import           Data.Singletons
import           Data.String                          (fromString)
import           Polysemy                             (Member, Sem, run)
import           Polysemy.Writer                      (Writer, runWriter, tell)

type TopLevelFunDefs = [Procedural.FunDef ByteString]

exampleModule1 :: Combinatory.Module '[ 'Combinatory.Foo2 , 'Combinatory.Foo1]
exampleModule1 = Combinatory.ModuleCons (Combinatory.Bind (sing @'Combinatory.Foo2) $ Combinatory.Lit $ Combinatory.StringLiteral "boo") $
                 -- TODO we are probably gonna want some convenience functions here.
                 Combinatory.ModuleCons (Combinatory.Bind (sing @'Combinatory.Foo1) $ Combinatory.Lit $ Combinatory.StringLiteral "hello")
                 Combinatory.ModuleNil

-- TODO make this compile
exampleModule2 :: Combinatory.Module '[ 'Combinatory.Foo2 , 'Combinatory.Foo1]
exampleModule2 = Combinatory.ModuleCons (Combinatory.Bind (sing @'Combinatory.Foo2) $ Combinatory.Lit $ Combinatory.StringLiteral "boo") $
                 Combinatory.ModuleCons (Combinatory.Bind (sing @'Combinatory.Foo1) $ concat)
                 Combinatory.ModuleNil
  where
    concat = Combinatory.Abs (sing @(Combinatory.ConcatContexts (Combinatory.SingletonContext Combinatory.Bar1)
                                     (Combinatory.SingletonContext Combinatory.Bar3)
                                    )
                             )
      (Combinatory.App (Combinatory.Prim "printf '%s%s' ") ( Combinatory.GenExprListCons (Combinatory.Var $ sing @'Combinatory.Bar1) $
                                                             Combinatory.GenExprListCons (Combinatory.Var $ sing @'Combinatory.Bar3) $
                                                             Combinatory.GenExprListNil)
      )

-- | Presumably the main entry point in this module
lowerModule :: Combinatory.Module (ss :: [Combinatory.Foo]) -> Procedural.Module ByteString
lowerModule = Combinatory.moduleFold f
  where
    f n b = n <> (Procedural.Module $ lowerOneTopLevelDefn b)

lowerOneTopLevelDefn :: forall s. Combinatory.TopLevelBind (s :: Combinatory.Foo) -> TopLevelFunDefs
lowerOneTopLevelDefn = uncurry snoc . run . runWriter @TopLevelFunDefs . Ids.runLocalNames @Ids.SimpleBashFunName . lowerTopLevelBind

lowerTopLevelBind :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                     , Member (Writer TopLevelFunDefs) r )
                  => Combinatory.TopLevelBind s -> Sem r (Procedural.FunDef ByteString)
lowerTopLevelBind (Combinatory.Bind i e) = do
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
  -- TODO this might be bad, i.e., running a concrete implementation
  -- in business code. Use 'bracket' and 'resources' here.
  n <- Ids.mkName @Ids.SimpleBashFunName "lambda"
  -- TODO generalize, so we can include a better name
  let mkName' = Ids.mkName @Ids.LocalBashVarName . Ids.LocalBashVarName . C8.pack . show
  -- TODO this should guarantee we are starting a new local scope
  vs <- traverse mkName' $ Combinatory.unContext $ fromSing c
  s <- lowerExpr e
  let f = Procedural.FunDef n vs s
  tell @TopLevelFunDefs [f]
  let a = Procedural.Application (Procedural.ClosureFromName n) []
  pure $ Procedural.Sequence [] a

lowerExprLet :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Ids.LocalNames Ids.LocalBashVarName) r
                , Member (Writer TopLevelFunDefs) r)
             => Combinatory.Bind s c -> Combinatory.Expr d -> Sem r (Procedural.Sequence ByteString)
lowerExprLet (Combinatory.Bind i e) f = do
  let mkName' = Ids.mkName @Ids.LocalBashVarName . Ids.LocalBashVarName . C8.pack . show
  n <- mkName' $ fromSing i
  b <- Procedural.Assignment n <$> lowerExpr e
  Procedural.Sequence s a <- lowerExpr f
  -- TODO this is not quite right. f needs to be able to capture
  -- i. but if i is already taken, we will get a different name back,
  -- and the capute would fail.
  --
  -- the question seems to be how to shadow correctly. we could use a
  -- naming effect to make sure that variable references in f are
  -- resolved correctly.
  pure $ Procedural.Sequence (b:s) a
  -- TODO implement a renaming effect


lowerExprVar :: Sing (s :: Combinatory.Foo) -> Sem r (Procedural.Sequence ByteString)
lowerExprVar n  = pure $ Procedural.Sequence [] $ Procedural.Variable $ Ids.LocalBashVarName $ C8.pack $ show $ fromSing n

lowerExpr :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
             , Member (Ids.LocalNames Ids.SimpleBashFunName) r
             , Member (Writer TopLevelFunDefs) r)
          => Combinatory.Expr c -> Sem r (Procedural.Sequence ByteString)
lowerExpr = \case
  Combinatory.Var n    -> lowerExprVar n
  Combinatory.Lit l    -> lowerExprLiteral l
  Combinatory.App e es -> lowerExprApp e es
  Combinatory.Abs c e  -> lowerExprAbs c e
  Combinatory.Prim n   -> lowerExprPrim n
  Combinatory.Let b e  -> lowerExprLet b e
