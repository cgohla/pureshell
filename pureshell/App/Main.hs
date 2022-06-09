{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module App.Main (main) where

import qualified Data.ByteString.Lazy.Char8 as B (hPutStrLn)
import           Data.Maybe                 (fromMaybe)
import           System.FilePath.Posix      ((-<.>))
import           System.IO                  (IOMode (..), hClose, openFile)

import           App.Driver                 (compileModule')
import           App.Options                (CLIOptions (..), runWithOptions)
import           Language.PureShell.Main    (getModule)

main :: IO ()
main = runWithOptions $ main' . getCompilerOptions

data CompilerOptions = CompilerOptions { inputPath  :: FilePath
                                       , outputPath :: FilePath
                                       }

getCompilerOptions :: CLIOptions -> CompilerOptions
getCompilerOptions CLIOptions{..} = CompilerOptions { outputPath = o, ..}
  where
    o = fromMaybe (inputPath -<.> "bash") outputPath

main' :: CompilerOptions -> IO ()
main' CompilerOptions{..} = do
  (_, m) <- getModule $ inputPath
  o <- openFile outputPath WriteMode
  B.hPutStrLn o $ compileModule' m
  hClose o
