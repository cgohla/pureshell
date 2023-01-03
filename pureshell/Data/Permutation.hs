{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Permutation where

import           Data.Singletons
import           Data.Singletons.Decide

import           Data.Nat

data Transp (n :: Nat) where
  Transp :: Sing n -> Transp ('S ('S n))
  Shift :: Transp n -> Transp ('S n)

data Perm (n :: Nat) where
  Ident :: Perm n
  TranspCons :: Transp n -> Perm n -> Perm n

compose :: Perm n -> Perm n -> Perm n
compose  Ident p            = p
compose  (TranspCons t p) q = TranspCons t $ compose p q

tensorIdLeftTransp :: forall m n. SingI n => Sing (m :: Nat) -> Transp n -> Transp (m `Add` n)
tensorIdLeftTransp SZ p = p
tensorIdLeftTransp (SS m) p = case lemSuccAddRight (SS m) (sing @n) of
                          Refl -> tensorIdLeftTransp m $ Shift p

tensorIdRightTransp :: Transp m -> Sing n -> Transp (m `Add` n)
tensorIdRightTransp (Transp m') n   = Transp (m' `sAdd` n)
tensorIdRightTransp (Shift t) n = Shift $ tensorIdRightTransp t n

tensorIdLeftPerm :: SingI n => Sing m -> Perm n -> Perm (m `Add` n)
tensorIdLeftPerm _m Ident = Ident
tensorIdLeftPerm m (TranspCons t p) = TranspCons (tensorIdLeftTransp m t) $ tensorIdLeftPerm m p

tensorIdRightPerm :: Perm m -> Sing n -> Perm (m `Add` n)
tensorIdRightPerm Ident _n = Ident
tensorIdRightPerm (TranspCons t p) n = TranspCons (tensorIdRightTransp t n) $ tensorIdRightPerm p n

tensor :: forall m n. (SingI m, SingI n) => Perm m -> Perm n -> Perm (m `Add` n)
tensor p q = compose (tensorIdLeftPerm (sing @m) q) (tensorIdRightPerm p (sing @n))
