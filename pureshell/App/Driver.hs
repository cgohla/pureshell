{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module App.Driver where

import           Data.ByteString.Builder                as B
import           Data.ByteString.Lazy                   as B
import           Data.ByteString.Lazy.Char8             as C8
import           Language.Bash.Script                   as Bash
import           Language.PureShell.Combinatory.Compile as C
import           Language.PureShell.Combinatory.Types   as C
import           Language.PureShell.Procedural          as P

outputModule :: C.Module (ss :: [Foo]) -> IO ()
outputModule = C8.putStrLn . compileModule

compileModule :: C.Module (ss :: [Foo]) -> B.ByteString
compileModule = B.toLazyByteString . Bash.script . P.toBashStatement . C.lowerModule
