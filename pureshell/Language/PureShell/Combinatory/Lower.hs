{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeApplications   #-}
module Language.PureShell.Combinatory.Lower where

import qualified Language.PureShell.Combinatory.CodeGen as C
import qualified Language.PureShell.Combinatory.Context as C
import qualified Language.PureShell.Combinatory.IR      as C
import qualified Language.PureShell.Identifiers         as Ids
import qualified Language.PureShell.Procedural.IR       as P

import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Char8                  as C8 (pack)
import           Data.List.Extra                        (snoc)
import           Data.Singletons
import qualified Data.Text                              as T (pack)
import qualified Data.Text.Encoding                     as T (encodeUtf8)
import           Polysemy                               (Member, Sem, run)
import           Polysemy.Writer                        (Writer, runWriter,
                                                         tell)

type TopLevelFunDefs = [P.FunDef ByteString]

-- | Presumably the main entry point in this module
lowerModule :: ( Ids.IdsKind ids)
            => C.Module ids (ss :: [ids]) -> P.Module ByteString
lowerModule = C.moduleFold f
  where
    f n b = n <> (P.Module $ lowerOneTopLevelDefn b)

lowerOneTopLevelDefn :: forall ids (s :: ids). (Ids.IdsKind ids) => C.TopLevelBind ids s -> TopLevelFunDefs
lowerOneTopLevelDefn = uncurry snoc . run . runWriter @TopLevelFunDefs . Ids.runLocalNames @Ids.SimpleBashFunName . lowerTopLevelBind
-- NOTE the function names effect might need to be run at the top of
-- the module, to guarantee function names are unique throughout it.

lowerTopLevelBind :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                     , Member (Writer TopLevelFunDefs) r
                     , Ids.IdsKind ids)
                  => C.TopLevelBind ids s -> Sem r (P.FunDef ByteString)
lowerTopLevelBind (C.Bind i e) = do
  fn <- Ids.mkName @Ids.SimpleBashFunName $ C.simpleBashFunName i
  let lowerExpr' = Ids.runLocalNames @Ids.LocalBashVarName . lowerExpr
  case e of
    C.Abs c f -> P.FunDef fn <$> ps <*> lowerExpr' f
      where
        ps = Ids.runLocalNames @Ids.LocalBashVarName $ varNamesFromContext c
    _ -> P.FunDef fn [] <$> t
      where
        t = lowerExpr' e

lowerExprLiteral :: ( Member (Writer TopLevelFunDefs) r
                    , Ids.IdsKind ids)
                 => C.Literal ids c -> Sem r (P.Sequence ByteString)
lowerExprLiteral = \case
  C.StringLiteral s         -> literal s
  C.NumericLiteral (Left n) -> literal $ T.pack $ show n
  _                         -> error "not implemented"
  where
    literal = C.expression . P.Literal . T.encodeUtf8

lowerExprApp :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Writer TopLevelFunDefs) r
                , Ids.IdsKind ids)
             => C.Expr ids c -> C.ExprList ids d -> Sem r (P.Sequence ByteString)
lowerExprApp e es = do
  (v, a) <- exprEvalAssign e -- TODO this produces wrong results in the case of Prim
  (vs, as) <- C.genExprListFold chainExprEval es -- TODO if an es is a Var then we should use it directly
  let b = P.Application (P.ClosureFromVar v) vs -- TODO if e is a Prim we may want to use the literal name
  C.sequence (a:as) b

exprEvalAssign :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                  , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                  , Member (Writer TopLevelFunDefs) r
                  , Ids.IdsKind ids)
               => C.Expr ids c -> Sem r (Ids.LocalBashVarName, P.Assignment ByteString)
exprEvalAssign e = do
  s <- lowerExpr e
  v <- Ids.mkName @Ids.LocalBashVarName "r"
  pure $ (v, P.Assignment v s)

chainExprEval :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                 , Member (Ids.LocalNames Ids.SimpleBashFunName) r
                 , Member (Writer TopLevelFunDefs) r
                 , Ids.IdsKind ids)
              => Sem r ([Ids.LocalBashVarName], [P.Assignment ByteString])
              -> C.Expr ids c -> Sem r ([Ids.LocalBashVarName], [P.Assignment ByteString])
chainExprEval as e = do
  (vs, bs) <- as
  (v, a) <-  exprEvalAssign e
  pure $ ([v] <> vs, [a] <> bs)

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

varNamesFromContext :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
                       , Ids.IdsKind ids)
                    => Sing (c :: C.Context ids)
                    -> Sem r [Ids.LocalBashVarName]
varNamesFromContext c = do
  let mkName' ns s = ns <> [Ids.mkName @Ids.LocalBashVarName $ C.localBashVarName s]
  sequence $ C.contextFoldl mkName' [] c

lowerExprAbs :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Writer TopLevelFunDefs) r
                , Ids.IdsKind ids)
             => Sing (c :: C.Context ids) -> C.Expr ids d -> Sem r (P.Sequence ByteString)
             -- NOTE we are not actually enforcing the binding
             -- constraint here anymore. maybe there is an easy way to
             -- do that
lowerExprAbs c e = Ids.runLocalNames @Ids.LocalBashVarName $ do
  -- TODO this might be bad, i.e., running a concrete implementation
  -- in business code. Use 'bracket' and 'resources' here.
  n <- Ids.mkName @Ids.SimpleBashFunName "lambda"
  -- TODO generalize, so we can include a better name
  vs <- varNamesFromContext c
  s <- lowerExpr e
  let f = P.FunDef n vs s
  tell @TopLevelFunDefs [f]
  let a = P.Application (P.ClosureFromName n) [] -- TODO the empty list seems wrong
  C.sequence [] a

lowerExprLet :: ( Member (Ids.LocalNames Ids.SimpleBashFunName) r
                , Member (Ids.LocalNames Ids.LocalBashVarName) r
                , Member (Writer TopLevelFunDefs) r
                , Ids.IdsKind ids)
             => C.Bind ids (s :: ids) c -> C.Expr ids d -> Sem r (P.Sequence ByteString)
lowerExprLet (C.Bind i e) f = do
  let mkName' = Ids.mkName @Ids.LocalBashVarName . C.localBashVarName
  n <- mkName' i
  b <- P.Assignment n <$> lowerExpr e
  P.Sequence s a <- lowerExpr f
  -- TODO this is not quite right. f needs to be able to capture
  -- i. but if i is already taken, we will get a different name back,
  -- and the capute would fail.
  --
  -- the question seems to be how to shadow correctly. we could use a
  -- naming effect to make sure that variable references in f are
  -- resolved correctly.
  C.sequence (b:s) a
  -- TODO implement a renaming effect

lowerExprVar :: (Ids.IdsKind ids) => Sing (s :: ids) -> Sem r (P.Sequence ByteString)
lowerExprVar = C.sequence [] . P.Variable . C.localBashVarName

lowerExpr :: ( Member (Ids.LocalNames Ids.LocalBashVarName) r
             , Member (Ids.LocalNames Ids.SimpleBashFunName) r
             , Member (Writer TopLevelFunDefs) r
             , Ids.IdsKind ids)
          => C.Expr ids c -> Sem r (P.Sequence ByteString)
lowerExpr = \case
  C.Var n    -> lowerExprVar n
  C.Lit l    -> lowerExprLiteral l
  C.App e es -> lowerExprApp e es
  C.Abs c e  -> lowerExprAbs c e
  C.Prim n   -> lowerExprPrim n
  C.Let b e  -> lowerExprLet b e
