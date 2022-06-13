{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
module Language.PureShell.Identifiers where

import           Data.Bifunctor        (second)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8 (pack)
import qualified Data.Map              as Map (Map, empty, insertLookupWithKey)
import           Data.Singletons
import           Data.String           (IsString, fromString)
import           Data.Text.Encoding    (encodeUtf8)
import           GHC.TypeLits
import           Polysemy              (InterpreterFor, Member, Sem, makeSem,
                                        reinterpret)
import           Polysemy.State        (State, evalState, get, put)

data LocalNames i m a where
  MkName :: i -> LocalNames i m i

makeSem ''LocalNames

class (Show i, Eq i, Ord i) => IsIdentifier i where
  reify :: i -> Maybe Integer -> i

class (SingKind ids) => IdsKind ids where
  toBS :: Sing (s :: ids) -> ByteString

newtype LocalBashVarName = LocalBashVarName { getVarName :: ByteString } deriving newtype (Show, Eq, Ord)

newtype SimpleBashFunName = SimpleBashFunName { getFunName :: ByteString } deriving newtype (Show, Eq, Ord)

instance IsIdentifier LocalBashVarName where
  reify (LocalBashVarName i) j = LocalBashVarName $ i <> maybe mempty (C8.pack . show) j

instance IsIdentifier SimpleBashFunName where
  reify (SimpleBashFunName i) j = SimpleBashFunName $ i <> maybe mempty (C8.pack . show) j

instance IsString LocalBashVarName where
  fromString = LocalBashVarName . fromString

instance IsString SimpleBashFunName where
  fromString = SimpleBashFunName . fromString

instance IdsKind Symbol where
  toBS = encodeUtf8 . fromSing

runLocalNames :: forall i r. (IsIdentifier i) => InterpreterFor (LocalNames i) r
runLocalNames = evalState (LocalNamesMap $ Map.empty @i) . reinterpret reinterpretLocalNames

newtype IsIdentifier i => LocalNamesMap i = LocalNamesMap { getMap :: Map.Map i Integer }
  deriving newtype (Show, Eq, Ord)

insertLookup :: (Ord k) => (v -> v -> v) -> k -> v -> Map.Map k v -> (Maybe v, Map.Map k v)
insertLookup f k v m = Map.insertLookupWithKey (\_ -> \x -> \y -> f x y) k v m

type Add t = t -> t -> t
insertLookupName :: (IsIdentifier i)
                 => Add Integer -> i -> Integer -> LocalNamesMap i
                 -> (Maybe Integer, LocalNamesMap i)
insertLookupName f i v (LocalNamesMap m) = second LocalNamesMap $ insertLookup f i v m

reinterpretLocalNames :: (IsIdentifier i, Member (State (LocalNamesMap i)) r')
                      => LocalNames i (Sem r) j -> Sem r' j
reinterpretLocalNames (MkName b) = do
  ns <- get
  let (i, ns') = insertLookupName (+) b 1 ns
  put ns'
  pure $ reify b i
