{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Language.PureShell.Combinatory.Shell where

import qualified Language.PureShell.Combinatory.IR    as C
import qualified Language.PureShell.Combinatory.Lower as C
import qualified Language.PureShell.Procedural.Lower  as P

import           Data.ByteString.Builder              (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8           as C8 (hPut)
import           Data.List                            (intercalate)
import qualified Language.Bash.Script                 as Bash
import           System.IO                            (hClose)
import           System.IO.Temp                       (withSystemTempFile)
import           System.Process                       (callCommand)

-- | Start a shell with a module loaded
shellWithModule :: C.Module (c :: [C.Foo]) -> IO ()
shellWithModule m = do
  let s = toLazyByteString $ Bash.script $ P.toBashStatement $ C.lowerModule m
  withSystemTempFile "pureshell-script-XXX" $ \t h -> do
    C8.hPut h $ "set -x" <> "\n"
    C8.hPut h s
    hClose h
    callCommand $ intercalate " " [ "bash", "--rcfile", t]



