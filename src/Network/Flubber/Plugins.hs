module Network.Flubber.Plugins
  ( MonadPlugin(..)
  , Plugin
  ) where

import Conduit (sourceHandle)
import Control.Lens ((^.), makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Network.Flubber.Config
  ( PluginConfig(..)
  , pluginConfigArgs
  , pluginConfigPath
  )
import Network.Flubber.Monad (FlubberT)
import System.IO (Handle)
import System.Process.Typed
  ( Process
  , ProcessConfig
  , createPipe
  , proc
  , setStdin
  , setStdout
  , startProcess
  )

class MonadPlugin m where
  spawnPlugin :: PluginConfig -> m Plugin

data Plugin = MkPlugin
  { _process :: Process Handle Handle ()
  } deriving Show

makeLenses ''Plugin

makeConfigFor :: PluginConfig -> ProcessConfig Handle Handle ()
makeConfigFor p =
  setStdin createPipe .
  setStdout createPipe $
  proc (p^.pluginConfigPath) (p^.pluginConfigArgs)

instance MonadIO m => MonadPlugin (FlubberT m) where
  spawnPlugin p = do
    let config = makeConfigFor p
    MkPlugin
      <$> startProcess config
