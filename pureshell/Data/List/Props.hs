{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module Data.List.Props ( IsElem(..)
                       , decideIsElem
                       , listRightUnit) where

import           Data.List.Singletons   (SList (..), type (++))
import           Data.Singletons        (Sing)
import           Data.Singletons.Decide (SDecide, decideEquality, type (:~:)(..))

data IsElem (x :: k) (xs :: [k]) where
  IsElemHead :: x :~: y -> IsElem x (y ': xs)
  IsElemTail :: IsElem x xs -> IsElem x (y ': xs)

deriving instance Show (IsElem x xs)

decideIsElem :: (SDecide k) => Sing (x :: k) -> Sing (xs :: [k]) -> Maybe (IsElem x xs)
decideIsElem _ SNil = Nothing
decideIsElem x (SCons y ys) = case decideEquality x y of
                                Nothing -> IsElemTail <$> decideIsElem x ys
                                Just r  -> Just $ IsElemHead r

listRightUnit :: Sing a -> a :~: (a ++ '[])
listRightUnit SNil = Refl
listRightUnit (SCons _ as) = case listRightUnit as of
                             Refl -> Refl
