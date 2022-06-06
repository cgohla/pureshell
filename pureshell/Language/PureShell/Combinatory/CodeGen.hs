{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeApplications   #-}
module Language.PureShell.Combinatory.CodeGen where

import qualified Language.PureShell.Procedural.IR as P

import           Polysemy                         (Sem)

import           Prelude                          hiding (sequence)

expression :: P.Expression l -> Sem r (P.Sequence l)
expression = sequence []

sequence :: [P.Assignment l] -> P.Expression l -> Sem r (P.Sequence l)
sequence as e = pure $ P.Sequence as e
