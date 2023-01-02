{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Vec where

import           Data.Nat

import           Data.Singletons
import           Data.Singletons.TH (genSingletons, promoteOnly, promote, genPromotions)

data Vec (n :: Nat) a where
  VecNil :: Vec 'Z a
  VecCons :: a -> Vec n a -> Vec ('S n) a

deriving instance (Show a) => Show (Vec n a)
deriving instance (Eq a) => Eq (Vec n a)
deriving instance (Ord a) => Ord (Vec n a)

genPromotions [''Vec]

naturals :: Sing n -> Vec n Nat
naturals = \case
  SZ     -> VecNil
  (SS n) -> VecCons (fromSing n) $ naturals n

instance Functor (Vec n) where
  fmap f = \case
    VecNil       -> VecNil
    VecCons a as -> VecCons (f a) $ fmap f as
