{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Language.PureShell.CoreFn.Lower -- ( lowerModule
                                       -- , lowerModuleThen
                                       -- )
where

import qualified Language.PureShell.Combinatory.IR as C
import qualified Language.PureShell.CoreFn.IR      as F
import qualified Language.PureShell.Identifiers    as Ids


import           Data.Singletons                   (Demote, Sing, SingKind,
                                                    SomeSing (..), fromSing,
                                                    sing, toSing)
import           Data.Singletons.Prelude.List      (SList (..))
import           Data.Singletons.Prelude.Monoid    (sMappend)
import qualified Data.Text                         as T (pack)
import           GHC.TypeLits                      (Symbol)

lowerModule :: F.Module a -> Maybe (SomeModule Symbol)
lowerModule = lowerModuleDecls . F.moduleDecls

-- | With this we don't need the caller to deal with destructing the module
-- existential.
lowerModuleThen :: F.Module a
                -> (forall ids ss. (Ids.IdsKind ids) => C.Module ids ss -> b)
                -> b
lowerModuleThen m f = case lowerModule m of
  Nothing                -> error "the module could not be compiled"
  Just (MkSomeModule m') -> f m'

lowerModuleDecls :: [F.Bind a] -> Maybe (SomeModule Symbol)
lowerModuleDecls = checkBinds . fmap lowerBind

checkBinds :: [SomeBind ids] -> Maybe (SomeModule ids)
checkBinds []                    = pure $ MkSomeModule C.ModuleNil
checkBinds (b:bs) = case checkBinds bs of
             Nothing                 -> Nothing
             Just (MkSomeModule m) -> case b of
               MkSomeBind b' _ (C.SContext SNil) ->
                 pure $ MkSomeModule $ C.ModuleCons b' m
               _ -> Nothing

lowerBind :: F.Bind a -> SomeBind Symbol
lowerBind (F.NonRec _ i e) = case lowerIdent i of
                               SomeSing s -> case lowerExpr e of
                                 MkSomeExpr e' c -> MkSomeBind (C.Bind s e') s c
lowerBind _                = error "recursive binds are not yet supported"

lowerExpr :: F.Expr a -> SomeExpr Symbol
lowerExpr = \case
  F.Literal _ l -> lowerLiteral l
  F.Abs a i e   -> lowerAbs a i e
--  F.App _ e e'  -> error "F.App is not supported yet"
  _             -> error "this expression is not yet handled"

lowerLiteral :: F.Literal (F.Expr a) -> SomeExpr Symbol
lowerLiteral = \case
  F.StringLiteral s ->
    let l = C.Lit $ C.StringLiteral $ T.pack $ F.decodeStringWithReplacement s in
    MkSomeExpr l $ sing @(C.EmptyContext)
  _ -> error "this literal is not yet handled"

-- TODO this is an abhomination, but apparently we can not derive an
-- SMonoid instance for Context
appendSContexts :: C.SContext (c :: C.Context ids)
                -> C.SContext (c' :: C.Context ids)
                -> C.SContext (c C.:<> c')
appendSContexts (C.SContext c) (C.SContext c') = C.SContext $ c `sMappend` c'

contextToVars :: Sing (c :: C.Context ids) -> C.ExprList ids c
contextToVars (C.SContext SNil)         = C.GenExprListNil
contextToVars (C.SContext (SCons v vs)) = error "TODO contextToVars"

removeOne :: (SingKind ids, Eq (Demote ids))
          => C.SContext (c :: C.Context ids)
          -> Sing (c' :: ids)
          -> SomeSing (C.Context ids)
removeOne c@(C.SContext SNil) _ = SomeSing c
removeOne (C.SContext (SCons s ss)) s' | fromSing s == fromSing s' = removeOne (C.SContext ss) s'
                                       | otherwise = case removeOne (C.SContext ss) s' of
                                                       SomeSing (C.SContext t) -> SomeSing $ C.SContext $ SCons s t

remove :: (SingKind ids, Eq (Demote ids))
       => C.SContext (c :: C.Context ids)
       -> C.SContext (c' :: C.Context ids)
       -> SomeSing (C.Context ids)
remove c@(C.SContext SNil) _ = SomeSing c
remove c@(C.SContext (SCons _ _)) (C.SContext (SCons s' ss')) = case removeOne c s' of
                                                                  SomeSing d -> remove d $ C.SContext ss'
remove c (C.SContext SNil) = SomeSing c

vsToContext :: [F.Ident] -> SomeSing (C.Context Symbol)
vsToContext [] = SomeSing $ C.SContext SNil
vsToContext (w:ws) = case lowerIdent w of
                       SomeSing w' -> case vsToContext ws of
                         SomeSing (C.SContext c) -> SomeSing $ C.SContext $ SCons w' c

lowerAbs :: ann -> F.Ident -> F.Expr ann -> SomeExpr Symbol
lowerAbs a i e = go [] $ F.Abs a i e
  where
    go vs (F.Abs _ i' e') = go (vs <> [i']) e'
    go vs e = case vsToContext vs of
                     SomeSing vs' -> case lowerExpr e of
                       MkSomeExpr g fvg -> case fvg `remove` vs' of
                         SomeSing fvg' -> MkSomeExpr (C.App (C.Abs (fvg' `appendSContexts` vs') g) $ contextToVars fvg') sing
                         -- TODO here we apparently need to somehow
                         -- provide evidence that the closure
                         -- constraint on the Abs is actually
                         -- satisfied

type SomeId = SomeSing Symbol

lowerIdent :: F.Ident -> SomeId
lowerIdent = \case
  F.Ident i -> toSing i
  _         -> error "this Ident is not handled yet"

-- | Existentials

data SomeModule ids where
  MkSomeModule :: C.Module ids ss -> SomeModule ids

data SomeExpr ids where
  MkSomeExpr :: C.Expr ids c -> Sing (c :: C.Context ids) -> SomeExpr ids

data SomeBind ids where
  MkSomeBind :: C.Bind ids (s :: ids) (c :: C.Context ids) -> Sing s -> Sing c -> SomeBind ids
