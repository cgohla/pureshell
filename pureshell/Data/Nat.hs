{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE TypeOperators #-}
module Data.Nat where

import           Data.Singletons.Base.TH

data Nat = Z | S Nat deriving (Show, Ord, Eq)

genSingletons [''Nat]

$(singletons [d|
               add :: Nat -> Nat -> Nat
               add Z n     = n
               add (S m) n = S (add m n)
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

natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S n) = 1 + natToInteger n
