{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module Data.Fin where

import           Data.Nat        (Add, Mult, N, Nat (..), SNat (..))

import           Data.Singletons (Sing)

data Fin (n :: Nat) where
  FinZ :: Sing n -> Fin n
  FinS :: Fin n -> Fin ((N 1) `Add` n)

deriving instance Show (Fin n)

-- | Take a singleton nat to the smallest containing fin set
fromNat :: Sing (n :: Nat) -> Fin ('S n)
fromNat SZ     = FinZ (SS SZ)
fromNat (SS n) = FinS (fromNat n)

finAddSing :: Sing (m :: Nat) -> Fin n -> Fin (m `Add` n)
finAddSing SZ n     = n
finAddSing (SS m) n = FinS $ finAddSing m n

finAdd :: Fin m -> Fin n -> Fin (m `Add` n)
finAdd (FinZ m) n = finAddSing m n
finAdd (FinS m) n = FinS $ finAdd m n

finMultSing :: Sing (m :: Nat) -> Fin n -> Fin (m `Mult` n)
finMultSing SZ _n    = FinZ SZ
finMultSing (SS m) n = finAdd n $ finMultSing m n

finMult :: Fin m -> Fin n -> Fin (m `Mult` n)
finMult (FinZ m) n = finMultSing m n
finMult (FinS m) n = finAdd n $ finMult m n

-- finDivMod :: Fin (m `Mult` n) -> Sing m -> (Fin m, Fin n)
-- finDivMod = undefined
