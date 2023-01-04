{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Permutation.Pretty where

import           Data.Permutation

import           Data.Nat
import           Data.Singletons
import           Text.PrettyPrint.ANSI.Leijen

natReplicate :: Nat -> a -> [a]
natReplicate Z _a    = []
natReplicate (S n) a = a : natReplicate n a

vertical :: Doc
vertical = char '|'

cross :: Doc
cross = text " ╳ "

prettyTransp :: Transp n -> Doc
prettyTransp (Transp n) = cross <+> (hsep $ natReplicate (fromSing n) vertical)
prettyTransp (Shift t)  = vertical <+> prettyTransp t

prettyPerm :: Perm n -> Doc
prettyPerm (Ident n)        = hsep $ natReplicate (fromSing n) vertical
prettyPerm (TranspCons t p) = vcat [prettyTransp t, prettyPerm p]

-- | prints
--
--   ╳  |
--  |  ╳
--  | | |
example :: Doc
example =  prettyPerm $
           compose (TranspCons (Transp sing) $ Ident sing) $
           tensor (Ident $ sing @('S 'Z)) $ TranspCons (Transp (sing)) $
           Ident $ sing @('S('S 'Z))
