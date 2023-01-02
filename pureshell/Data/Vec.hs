{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
module Data.Vec where

import Data.Nat

data Vec (n :: Nat) a where
  VecNil :: Vec 'Z a
  VecCons :: a -> Vec n a -> Vec ('S n) a
