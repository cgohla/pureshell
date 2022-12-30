{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
module App.Options ( runWithOptions
                   , CompilerCLIOptions(..)
                   , DebugCLIOptions(..)) where

import           Control.Monad.IO.Class (liftIO)
import           Options.Declarative

data CompilerCLIOptions = CompilerCLIOptions { inputPath  :: FilePath
                                             , outputPath :: Maybe FilePath
                                             }

runWithOptions :: (CompilerCLIOptions -> IO ()) -> (DebugCLIOptions -> IO ()) -> IO ()
runWithOptions f g = run "pureshell" Nothing $ Group "pureshell" [ subCmd "compile" $ getCompilerCLIOptions f
                                                                 , subCmd "debug" $ getDebugCLIOptions g
                                                                 ]

getCompilerCLIOptions :: (CompilerCLIOptions -> IO ())
              -> Arg "INPUT" FilePath
              -> Flag "o" '[] "OUTPUT" "" (Maybe FilePath)
              -> Cmd "compile PureScript CoreFn to Bash" ()
getCompilerCLIOptions f i o = liftIO $ f $ CompilerCLIOptions (get i) (get o)

data DebugCLIOptions = DebugCLIOptions { inputPath :: FilePath
                                       , showContext :: Bool
                                       }

getDebugCLIOptions :: (DebugCLIOptions -> IO ())
                   -> Arg "INPUT" FilePath
                   -> Flag "c" '["show-contexts"] "" "" Bool
                   -> Cmd "debug the compiler" ()
getDebugCLIOptions f i s = liftIO $ f $ DebugCLIOptions (get i) (get s)
