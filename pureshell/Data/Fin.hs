{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.Fin where

import GHC.TypeLits

data Fin (n :: Nat) where
  FinZ :: Fin n
  FinS :: Fin n -> Fin (1 + n)

