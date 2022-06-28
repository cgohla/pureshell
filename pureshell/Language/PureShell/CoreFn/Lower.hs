{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
module Language.PureShell.CoreFn.Lower ( lowerModule
                                       , lowerModuleThen
                                       ) where

import qualified Language.PureShell.Combinatory.IR as C
import qualified Language.PureShell.CoreFn.IR      as F
import qualified Language.PureShell.Identifiers    as Ids


import           Data.Singletons                   (Sing, SomeSing (..), sing,
                                                    toSing)
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
  F.Abs _ i e   -> lowerAbs i e
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
contextToVars (C.SContext (SCons v vs)) = undefined

lowerAbs :: F.Ident -> F.Expr ann -> SomeExpr Symbol
lowerAbs i e@(F.Abs _ _ _) = undefined -- TODO this should probably a clause in a \case
lowerAbs i e = case vs of
                 SomeSing vs' -> case lowerExpr e of
                   MkSomeExpr g fvg -> MkSomeExpr e' sing -- TODO compute the context (???)
                     where
                       e' = C.App (C.Abs (fvg' `appendSContexts` vs') g) $ contextToVars fvg' -- TODO leave off the App when fvg' is empty
                       -- IDEA maybe that shoud be an invariant on Combinatory, that App and Abs have to be over at least one variable
                       fvg' = undefined -- TODO this needs to have only the symbols that are not bound, i.e., fvg minus vs'
  where
    vs = case lowerIdent i of
      SomeSing i' -> SomeSing $ C.SContext $ SCons i' SNil

  -- let's try doing it one var at a time first

  -- we need a function that can recurse if e is another abs, otherwise lower the e
  -- we also need to take care to collect all the free variables in e

  -- error "F.Abs is not supported yet"

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
