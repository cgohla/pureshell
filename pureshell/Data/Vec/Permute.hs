{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
module Data.Vec.Permute where

import           Data.Permutation   (Perm (Ident, TranspCons),
                                     Transp (Shift, Transp))
import           Data.Vec

import           Data.Singletons.TH

$(promote [d|
            transpose :: Transp n -> Vec n a -> Vec n a
            transpose (Transp _n) (VecCons a (VecCons a' as)) = VecCons a' ((VecCons a) as)
            transpose (Shift t) (VecCons a as)                = VecCons a (transpose t as)

            permute :: Perm n -> Vec n a -> Vec n a
            permute (Ident _n) v       = v
            permute (TranspCons t p) v = transpose t (permute p v)
            |]
 )
