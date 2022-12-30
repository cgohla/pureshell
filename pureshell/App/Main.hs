{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module App.Main (main) where

import qualified Data.ByteString.Lazy.Char8                     as B (hPutStrLn)
import           Data.Maybe                                     (fromMaybe)
import           System.FilePath.Posix                          ((-<.>))
import           System.IO                                      (IOMode (..),
                                                                 hClose,
                                                                 hPutStr,
                                                                 openFile)

import           App.Driver                                     (compileModule')
import           App.Options                                    (CompilerCLIOptions (..),
                                                                 DebugCLIOptions (..),
                                                                 runWithOptions)
import           Language.PureShell.ContextCoreFn.Pretty        (prettyModule)
import           Language.PureShell.CoreFn.LowerToContextCoreFn (lowerModule)
import           Language.PureShell.Main                        (getModule)
import           Text.PrettyPrint.ANSI.Leijen                   (hPutDoc)

main :: IO ()
main = runWithOptions (compilerMain . getCompilerOptions) (debugMain . getDebugOptions)

data CompilerOptions = CompilerOptions { inputPath  :: FilePath
                                       , outputPath :: FilePath
                                       }

data DebugOptions = DebugOptions { inputPath   :: FilePath
                                 , outputPath  :: FilePath
                                 , showContext :: Bool
                                 }

getCompilerOptions :: CompilerCLIOptions -> CompilerOptions
getCompilerOptions CompilerCLIOptions{..} = CompilerOptions { outputPath = o, ..}
  where
    o = fromMaybe (inputPath -<.> "bash") outputPath

compilerMain :: CompilerOptions -> IO ()
compilerMain CompilerOptions{..} = do
  (_, m) <- getModule $ inputPath
  o <- openFile outputPath WriteMode
  B.hPutStrLn o $ compileModule' m
  hClose o

getDebugOptions :: DebugCLIOptions -> DebugOptions
getDebugOptions DebugCLIOptions{..} = DebugOptions { outputPath = o, ..}
  where
    o = inputPath -<.> "context-corefn"

debugMain :: DebugOptions -> IO ()
debugMain DebugOptions{..} = do
  (_, m) <- getModule $ inputPath
  o <- openFile outputPath WriteMode
--   B.hPutStrLn o $ compileModule' m
  hPutDoc o $ prettyModule showContext $ lowerModule m
  hPutStr o "\n"
  hClose o

