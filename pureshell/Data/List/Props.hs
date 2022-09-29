{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Data.List.Props ( IsElem(..)
                       , decideIsElem
                       , listRightUnit
                       , pLookup
                       , SomeIsElem(..)
                       , pLookup'
                       , Keys
                       , pLookup''
                       , SomeElemIsElem(..)
                       , fmapDist
                       , consDist
                       , sKeys
                       , keys
                       ) where

import           Data.Functor.Singletons (FmapSym0, PFunctor (..), sFmap)
import           Data.List.Singletons    (SList (..), type (++))
import           Data.Singletons         (Sing, SomeSing (..))
import           Data.Singletons.Decide  (SDecide, decideEquality,
                                          type (:~:) (..))
import           Data.Singletons.TH      (singletons)
import           Data.Tuple.Singletons   (FstSym0, STuple2 (..), sFst)

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

-- type Keys a = Fmap FstSym0 a

$(singletons [d|
               keys :: [(a, b)] -> [a]
               keys = fmap fst
               |]
 )

pLookup :: IsElem x (Keys d)
        -> Sing (d :: [(k, v)])
        -> SomeSing v
pLookup (IsElemHead Refl) (SCons (STuple2 _ v) _)
  = SomeSing v
pLookup (IsElemTail q) (SCons _ d)
  = pLookup q d

data SomeIsElem x xs =
  forall y. SomeIsElem (IsElem '(x, y) xs)

pLookup' :: IsElem x (Keys d)
         -> Sing (d :: [(k, v)])
         -> SomeIsElem x d
pLookup' (IsElemHead Refl) (SCons (STuple2 _ _) _) =
  SomeIsElem $ IsElemHead Refl
pLookup' (IsElemTail q) (SCons _ d) =
  case pLookup' q d of
    SomeIsElem p -> SomeIsElem $ IsElemTail p

data SomeElemIsElem x xs =
  forall y. SomeElemIsElem (IsElem '(x, y) xs) (Sing y)

pLookup'' :: IsElem x (Keys d)
          -> Sing (d :: [(k, v)])
          -> SomeElemIsElem x d
pLookup'' (IsElemHead Refl) (SCons (STuple2 _ v) _) =
  SomeElemIsElem (IsElemHead Refl) v
pLookup'' (IsElemTail q) (SCons _ d) =
  case pLookup'' q d of
    SomeElemIsElem p v -> SomeElemIsElem (IsElemTail p) v

-- (p x :~: 'True) -> Filter p (x ': xs) :~: x ': (Filter p xs)

-- IsElem x xs -> p x :~: 'True -> IsElem x (Filter )

consDist :: a :~: b -> (x ': a) :~: (x ': b)
consDist Refl = Refl

fmapDist :: Sing f
         -> Sing a
         -> Sing b
         -> (Fmap f (a ++ b)) :~: ((Fmap f a) ++ (Fmap f b))
fmapDist _ SNil _         = Refl
fmapDist f (SCons _ as) b = consDist $ fmapDist f as b
