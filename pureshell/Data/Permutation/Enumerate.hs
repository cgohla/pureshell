{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Data.Permutation.Enumerate where

import           Data.Fin         (Fin (..))
import           Data.Nat         (Fact, Nat (..), SNat (..))
import           Data.Permutation (Perm (..))

import           Data.Singletons  (Sing)

perm :: Sing n -> Fin (Fact n) -> Perm n
perm n (FinZ _)      = Ident n
perm (SS n) (FinS m) = undefined
