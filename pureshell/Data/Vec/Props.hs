{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
module Data.Vec.Props where

import           Data.Singletons        (Sing)
import           Data.Singletons.Decide (type (:~:))


import           Data.Permutation
import           Data.Vec
import qualified Data.Vec.Permute       as Vec



data IsElem (x :: k) (xs :: Vec n k) where
  IsElemHead :: x :~: y -> IsElem x ('VecCons y xs)
  IsElemTail :: IsElem x xs -> IsElem x ('VecCons y xs)

deriving instance Show (IsElem x xs)

-- This can't be implemented because we don't have promoted Transp
-- constructors, and hence no singletons.
-- c.f. Data.Permutation
permuteTransp :: Sing (t :: Transp n)
              -> IsElem x (xs :: Vec n k)
              -> IsElem x (Vec.Transpose t xs)
permuteTransp = undefined

-- idem
permute :: Sing (p :: Perm n)
        -> IsElem x (xs :: Vec n k)
        -> IsElem x (Vec.Permute p xs)
permute = undefined
