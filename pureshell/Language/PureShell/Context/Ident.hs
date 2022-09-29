{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE ExplicitNamespaces       #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE IncoherentInstances      #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverlappingInstances     #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module Language.PureShell.Context.Ident where

import           Data.Bool.Singletons
import           Data.Eq.Singletons
import           Data.Functor.Singletons
import           Data.List.Singletons
import           Data.Maybe.Singletons      (JustSym0, NothingSym0, SMaybe (..))
import           Data.Ord.Singletons
import           Data.Singletons.TH         (genSingletons, singletons)
import           Data.Singletons.TH.Options (defaultOptions,
                                             defunctionalizedName,
                                             promotedDataTypeOrConName,
                                             withOptions)
import           Data.Text                  (Text)
import           GHC.TypeLits               (Nat)
import           GHC.TypeLits.Singletons    (Symbol)
import           Language.Haskell.TH        (Name)
import           Text.Show.Singletons       ()

-- | Identifiers shared among the AST types with context

data InternalIdentData
  = RuntimeLazyFactory
  | Lazy !Text
--   deriving (Show, Eq, Ord)

-- | Unfortunately we can not use Language.PureScript.Names.ModuleName
-- here because in order to generate SDecide instances, we must be
-- able to derive our own Eq instance in the TH splice below.
newtype ModuleName = ModuleName Text
-- | We need this custom promoted version ModuleName. The `P` prefixed
-- names are needed unfortunately to make the promotion hack below
-- work.
newtype PModuleName = PModuleName Symbol
--   deriving (Ord, Eq, Show)

data Imported a = Imported ModuleName a
data PImported a = PImported PModuleName a

data Qualified a = Qualified (Maybe ModuleName) a
--  deriving (Ord, Eq, Show)
data PQualified a = PQualified (Maybe PModuleName) a
--  deriving (Ord, Eq, Show)

type PQIdent = PQualified PIdent

-- | We can't reuse the def'n from Language.PureScript.Names because
-- that uses Integers instead of Nats.
data Ident
   = Ident Text
   | GenIdent (Maybe Text) Nat
   | UnusedIdent
--   deriving (Ord, Eq, Show)

data PIdent
   = PIdent Symbol
   | PGenIdent (Maybe Symbol) Nat
   | PUnusedIdent
--   deriving (Ord, Eq, Show)

$(let
     customPromote :: Name -> Name
     customPromote n
       | n == ''Imported = ''PImported
       | n == 'Imported = 'PImported
       | n == ''Ident  = ''PIdent
       | n == 'Ident = 'PIdent
       | n == 'GenIdent = 'PGenIdent
       | n == 'UnusedIdent = 'PUnusedIdent
       | n == ''Text     = ''Symbol
       | n == ''ModuleName = ''PModuleName
       | n == 'ModuleName = 'PModuleName
       | n == ''Qualified = ''PQualified
       | n == 'Qualified = 'PQualified
       | otherwise       = promotedDataTypeOrConName defaultOptions n

     customDefun :: Name -> Int -> Name
     customDefun n sat = defunctionalizedName defaultOptions (customPromote n) sat
 in
    withOptions defaultOptions{ promotedDataTypeOrConName = customPromote
                              , defunctionalizedName      = customDefun
                              } $
    do
      let ts = [''Ident, ''ModuleName, ''Qualified, ''Imported]
      a <- genSingletons ts
      c <- singletons [d|
                        local :: a -> Qualified a
                        local = Qualified Nothing
                        locals :: Functor f => f a -> f (Qualified a)
                        locals = fmap local
                        import_ :: Imported a -> Qualified a
                        import_ (Imported m i) = Qualified (Just m) i
                        imports :: Functor f => f (Imported a) -> f (Qualified a)
                        imports = fmap import_

                        deriving instance Eq Ident
                        deriving instance Eq ModuleName
                        deriving instance (Eq a) => Eq (Qualified a)
                        deriving instance (Eq a) => Eq (Imported a)

                        -- TODO awaiting solution of
                        -- https://github.com/goldfirere/singletons/issues/538
                        -- deriving instance Show Ident
                        -- deriving instance Show ModuleName
                        -- deriving instance (Show a) => Show (Qualified a)
                        -- deriving instance (Show a) => Show (Imported a)

                        deriving instance Ord Ident
                        deriving instance Ord ModuleName
                        deriving instance (Ord a) => Ord (Qualified a)
                        deriving instance (Ord a) => Ord (Imported a)

                        deriving instance Functor Qualified
                        deriving instance Functor Imported

                        |]
      return $ a <> c
 )
