{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Data.Permutation where

import           Data.Singletons
import           Data.Singletons.Decide

import           Data.Nat
import           Data.Singletons.TH     (genSingletons)

data Transp (n :: Nat) where
  Transp :: Sing n -> Transp ('S ('S n))
  Shift :: Transp n -> Transp ('S n)

data Perm (n :: Nat) where
  Ident :: Sing n -> Perm n
  TranspCons :: Transp n -> Perm n -> Perm n

-- genSingletons [''Transp, ''Perm]
-- This does not work, because GADT data constructors can not be promoted
-- But see

--       Stephanie Weirich, Justin Hsu, Richard A. Eisenberg - Down with kinds:
--       adding dependent heterogeneous equality to FC (Extended Version)

-- where they propose implementing the needed kind equalities. Unknown
-- whether this is implemented in GHC.

compose :: Perm n -> Perm n -> Perm n
compose  (Ident _n) p       = p
compose  (TranspCons t p) q = TranspCons t $ compose p q

tensorIdLeftTransp :: Sing (m :: Nat) -> Sing n -> Transp n -> Transp (m `Add` n)
tensorIdLeftTransp SZ _n p = p
tensorIdLeftTransp (SS m) n p = case lemSuccAddRight (SS m) n of
                                  Refl -> tensorIdLeftTransp m (SS n) $ Shift p

tensorIdRightTransp :: Transp m -> Sing n -> Transp (m `Add` n)
tensorIdRightTransp (Transp m') n = Transp (m' `sAdd` n)
tensorIdRightTransp (Shift t) n   = Shift $ tensorIdRightTransp t n

tensorIdLeftPerm :: Sing m -> Sing n -> Perm n -> Perm (m `Add` n)
tensorIdLeftPerm m _n (Ident n) = Ident $ m `sAdd` n
tensorIdLeftPerm m n (TranspCons t p) = TranspCons (tensorIdLeftTransp m n t) $ tensorIdLeftPerm m n p

tensorIdRightPerm :: Perm m -> Sing n -> Perm (m `Add` n)
tensorIdRightPerm (Ident m) n = Ident $ m `sAdd` n
tensorIdRightPerm (TranspCons t p) n = TranspCons (tensorIdRightTransp t n) $ tensorIdRightPerm p n

tensor :: forall m n. (SingI m, SingI n) => Perm m -> Perm n -> Perm (m `Add` n)
tensor p q = compose (tensorIdLeftPerm (sing @m) (sing) q) (tensorIdRightPerm p (sing @n))

tensor' :: forall m n. Sing m -> Sing n -> Perm m -> Perm n -> Perm (m `Add` n)
tensor' m n p q = compose (tensorIdLeftPerm m n q) (tensorIdRightPerm p n)

permCycle :: Sing n -> Perm n
permCycle = \case
  SZ -> Ident SZ
  SS SZ -> Ident $ SS SZ
  n'@(SS n''@(SS n)) -> compose (TranspCons (Transp n) $ Ident n') $
                        tensor' sing n'' (Ident $ SS SZ) $ permCycle n''
