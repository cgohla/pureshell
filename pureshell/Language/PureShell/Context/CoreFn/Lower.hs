{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Language.PureShell.Context.CoreFn.Lower where

-- | Lower Context.CoreFn to Context.Split

import qualified Language.PureShell.Context.CoreFn.IR as X
import           Language.PureShell.Context.Ident
import qualified Language.PureShell.Context.Split.IR  as S

import           Data.Functor.Singletons
import           Data.List.Singletons
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Tuple.Singletons


lowerModule :: X.Module a -> S.Module a
lowerModule X.Module{..} = case funVarsSection (sImports moduleImports %++ sLocals moduleForeign) of
                             -- REMEMBER We have to actually match the
                             -- Refl; using a wildcard is not enough.
                             Refl -> S.Module { S.moduleDecls = lowerBindList moduleDecls
                                              , ..
                                              }

type Keys a = Fmap FstSym0 a

ap :: (a :~: b) -> f a :~: f b
ap Refl = Refl

funVarsSection :: Sing (a :: [k]) -> Keys (S.FunVars a) :~: a
funVarsSection SNil         = Refl
funVarsSection (SCons _ as) = ap $ funVarsSection as

funVarSection :: Sing a -> Fst (S.FunVar a) :~: a
funVarSection _ = Refl

lowerBindList :: X.BindList a l (Keys c)
              -> S.BindList a (S.FunVars l) c
lowerBindList = error "not implemented"
