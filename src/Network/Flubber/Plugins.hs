module Network.Flubber.Plugins
  ( MonadPlugin(..)
  , Plugin
  ) where

import Conduit (ConduitT, (.|), runConduit, sinkHandle, sourceHandle)
import Control.Lens ((^.), makeLenses)
import Control.Monad.IO.Class (MonadIO)
import Network.Flubber.Config
  ( PluginConfig(..)
  , pluginConfigArgs
  , pluginConfigPath
  )
import Network.Flubber.Monad (FlubberT)
import Network.Flubber.Plugins.Types (Request, Response, Update)
import Network.Flubber.Utils (conduitFromJSON, conduitToJSON, conduitXlatJSON)
import System.IO (Handle)
import System.Process.Typed
  ( Process
  , ProcessConfig
  , createPipe
  , getStdin
  , getStdout
  , proc
  , setStdin
  , setStdout
  , startProcess
  )

class MonadPlugin m where
  spawnPlugin :: PluginConfig -> m ( Plugin
                                   , ConduitT () (Either Response Update) m ()
                                   , ConduitT Request () m ())

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
    process <- startProcess config
    let stdinValue = sourceHandle (getStdin process) .| conduitFromJSON
    let stdout = conduitToJSON .| sinkHandle (getStdout process)
    infoValue <- runConduit stdinValue
    let stdin = stdin .| conduitXlatJSON
    pure (MkPlugin process, stdin, stdout)
