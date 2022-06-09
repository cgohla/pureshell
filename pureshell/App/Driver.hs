{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
module App.Driver where

import           Data.ByteString.Builder              as B
import           Data.ByteString.Lazy                 as B
import           Data.ByteString.Lazy.Char8           as C8

import           Language.Bash.Script                 as Bash
import           Language.PureShell.Combinatory.IR    as C
import           Language.PureShell.Combinatory.Lower as C
import qualified Language.PureShell.CoreFn.IR         as F
import qualified Language.PureShell.CoreFn.Lower      as F
import           Language.PureShell.Identifiers       as Ids
import           Language.PureShell.Procedural.Lower  as P

outputModule :: Ids.IdsKind ids => C.Module ids (ss :: [ids]) -> IO ()
outputModule = C8.putStrLn . compileModule

compileModule :: Ids.IdsKind ids =>  C.Module ids (ss :: [ids]) -> B.ByteString
compileModule = B.toLazyByteString . Bash.script . P.toBashStatement . C.lowerModule

compileModule' :: F.Module a -> B.ByteString
compileModule' m = F.lowerModuleThen m compileModule

outputModule' :: F.Module a -> IO ()
outputModule' = C8.putStrLn . compileModule'
