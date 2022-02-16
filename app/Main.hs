{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import qualified Data.List.NonEmpty as NE (nonEmpty, head)
import Data.Maybe (maybe)
import qualified Data.ByteString.Lazy as B (readFile, putStrLn)
import Data.Version (Version)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)

import Language.PureShell (convert)
import Language.PureShell.Main (getModule)

main :: IO ()
main = do
  opts <- fmap NE.nonEmpty getArgs
  file <- case opts of
    Nothing -> error "No input file"
    Just f -> pure $ NE.head f
  (_, m) <- getModule file
  B.putStrLn $ convert m
