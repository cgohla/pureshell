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

import qualified Language.PureShell.Identifiers   as Ids
import qualified Language.PureShell.Procedural.IR as P

import           Data.ByteString                  as BS (ByteString, pack,
                                                         unpack)
import qualified Data.ByteString.Char8            as C8 (pack)
import           Data.ByteString.Internal         as BS (c2w)
import           Data.Singletons
import           Polysemy                         (Sem)
import           Prelude                          hiding (sequence)

expression :: P.Expression l -> Sem r (P.Sequence l)
expression = sequence []

sequence :: [P.Assignment l] -> P.Expression l -> Sem r (P.Sequence l)
sequence as e = pure $ P.Sequence as e

pEncode :: BS.ByteString -> BS.ByteString
pEncode b = BS.pack $ foldMap subst $ BS.unpack b
  where
    subst i = if or [i <= 47 , and [58 <= i, i <= 64], and [ 91 <= i, i <= 96], 123 <= i]
              then
                fmap BS.c2w $ "p" <> (show $ fromEnum i)
              else
                pure i

localBashVarName :: (Ids.IdsKind ids) => Sing (s :: ids) -> Ids.LocalBashVarName
localBashVarName = Ids.LocalBashVarName . pEncode . Ids.toBS

simpleBashFunName :: (Ids.IdsKind ids) => Sing (s :: ids) -> Ids.SimpleBashFunName
simpleBashFunName = Ids.SimpleBashFunName . pEncode . Ids.toBS
