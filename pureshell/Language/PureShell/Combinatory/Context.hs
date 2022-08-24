{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeApplications   #-}
module Language.PureShell.Combinatory.Context where

import           Language.PureShell.Combinatory.IR

import           Data.Singletons
import           Data.List.Singletons

-- | Consume a context

contextFoldl :: (forall s . b -> Sing (s :: ids) -> b)  -> b -> Sing (c :: Context ids) -> b
contextFoldl f b (SContext c) = go f b c
  where
    go :: (forall (s :: k) . b -> Sing s -> b)  -> b -> SList (ss :: [k]) -> b
    go _ b' SNil         = b'
    go f' b' (SCons h t) = go f' (f' b' h) t

