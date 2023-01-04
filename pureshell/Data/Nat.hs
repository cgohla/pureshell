{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Data.Nat where

import           Data.Singletons.Base.TH
import           GHC.TypeLits            (Natural, type (-))
-- import qualified GHC.TypeLits.Singletons as TS (Natural)

data Nat = Z | S Nat deriving (Ord, Eq)

-- | facilitate entering type level nats
-- ghci> :kind!  N 3
-- N 3 :: Nat
-- = 'S ('S ('S 'Z))
type family N (n :: Natural) :: Nat where
  N 0 = 'Z
  N n = 'S (N (n - 1))

genSingletons [''Nat]

$(singletons [d|
               add :: Nat -> Nat -> Nat
               add Z n     = n
               add (S m) n = S (add m n)

               mult :: Nat -> Nat -> Nat
               mult Z _     = Z
               mult (S m) n = n `add` (mult m n)

               fact :: Nat -> Nat
               fact Z     = S Z
               fact (S n) = (S n) `mult` (fact n)

               deriving instance (Show Nat)

               -- literal patterns are not supported yet by the singletons library
               --
               -- nN :: TS.Natural -> Nat
               -- nN = \case
               --   0 -> Z
               --   n -> nN (n - 1)
               |]
 )

lemSuccAddLeft :: Sing m -> Sing n -> 'S(m `Add` n) :~: 'S m `Add` n
lemSuccAddLeft SZ _n = Refl
lemSuccAddLeft (SS m) n = case lemSuccAddLeft m n of
                            Refl -> Refl

lemSuccAddRight :: Sing m -> Sing n -> 'S(m `Add` n) :~: m `Add` 'S n
lemSuccAddRight SZ _n = Refl
lemSuccAddRight (SS m) n = case lemSuccAddRight m n of
                             Refl -> Refl

factMult :: Sing ('S m) -> Sing (Fact m) -> Sing (Fact ('S m))
factMult sm fm = sm `sMult` fm

factFact :: Sing (Fact ('S m)) -> (Sing ('S m), Sing (Fact m))
factFact = undefined

natToInteger :: Nat -> Integer
natToInteger Z     = 0
natToInteger (S n) = 1 + natToInteger n

lemZeroMult :: Sing n -> Sing ('Z `Mult` n) :~: Sing 'Z
lemZeroMult SZ     = Refl
lemZeroMult (SS n) = lemZeroMult n

instance Num Nat where
  (+) = add
  (*) = mult
  (-) = error "minus is not implemented for Nat"
  negate = error "negate is  not implemented for Nat"
  abs = id
  signum = const 1
  fromInteger = \case
    0 -> Z
    n -> S (fromInteger $ n - 1)

natFact :: Sing (('S m) `Mult` n) -> Sing ('S m) -> Sing n
natFact k (SS m) = undefined -- ???
