{-# LANGUAGE TypeApplications #-}
module Language.PureShell.Main where

import System.Environment (getArgs)
import qualified Data.List.NonEmpty as NE (nonEmpty, head)
import Data.Maybe (maybe)
import qualified Data.ByteString.Lazy as B (readFile)
import Data.Version (Version)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (putStrLn)

import Data.Aeson.Types (parseEither, Value)
import Data.Aeson (decode)
import Data.Either (either)

import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Expr (Expr)
import Language.PureScript.Names (Ident)
import Language.PureScript.PSString (PSString)
import Language.PureScript.CoreFn.FromJSON (moduleFromJSON)
import Language.PureScript.CoreFn.Module (Module(..))

import Language.PureScript.AST.Literals (Literal)

getModule :: FilePath -> IO (Version, Module Ann)
getModule p = do
  b <- B.readFile p
  let jsonError = error $ "Could not parse JSON data in file: " <> p
  r <- (maybe jsonError pure) $ decode @Value b
  let getModule' = either error id . parseEither moduleFromJSON
  pure $ getModule' r
