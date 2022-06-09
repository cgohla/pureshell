{-# LANGUAGE DataKinds #-}
module App.Options ( runWithOptions
                   , CLIOptions(..)) where

import           Control.Monad.IO.Class (liftIO)
import           Options.Declarative

data CLIOptions = CLIOptions { inputPath  :: FilePath
                             , outputPath :: Maybe FilePath
                             }

runWithOptions :: (CLIOptions -> IO ()) -> IO ()
runWithOptions f = run "pureshell" Nothing $ getCLIOptions f

getCLIOptions :: (CLIOptions -> IO ())
              -> Arg "INPUT" FilePath
              -> Flag "o" '[] "OUTPUT" "" (Maybe FilePath)
              -> Cmd "compile PureScript CoreFn to Bash" ()
getCLIOptions f i o = liftIO $ f $ CLIOptions (get i) (get o)
