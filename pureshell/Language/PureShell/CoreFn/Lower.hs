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


import           Data.List.Singletons              (SList (..))
import           Data.Singletons                   (Sing, SomeSing (..), sing,
                                                    toSing)
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
  _             -> error "this expression is not yet handled"

lowerLiteral :: F.Literal (F.Expr a) -> SomeExpr Symbol
lowerLiteral = \case
  F.StringLiteral s ->
    let l = C.Lit $ C.StringLiteral $ T.pack $ F.decodeStringWithReplacement s in
    MkSomeExpr l $ sing @(C.EmptyContext)
  _ -> error "this literal is not yet handled"

type SomeId = SomeSing Symbol

lowerIdent :: F.Ident -> SomeId
lowerIdent = \case
  F.Ident i -> toSing i
  _         -> error "this Ident is not handled yet"

-- | Existential

data SomeModule ids where
  MkSomeModule :: C.Module ids ss -> SomeModule ids

data SomeExpr ids where
  MkSomeExpr :: C.Expr ids c -> Sing (c :: C.Context ids) -> SomeExpr ids

data SomeBind ids where
  MkSomeBind :: C.Bind ids (s :: ids) (c :: C.Context ids) -> Sing s -> Sing c -> SomeBind ids
