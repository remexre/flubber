module Network.Flubber.Plugins
  ( MonadPlugin(..)
  , Plugin
  , pluginInitInfo
  , PluginMisbehavior(..)
  , Req(..)
  ) where

import Conduit -- ((.|), headC, runConduit, sinkHandle, sourceHandle)
import Control.Concurrent.MVar (newEmptyMVar, readMVar)
import Control.Concurrent.STM.TBMChan (newTBMChan, writeTBMChan)
import Control.Lens ((.=), (^.), (^?), at, ix, preuse)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (atomically)
import Data.Aeson (FromJSON(..), Value, fromJSON)
import qualified Data.Aeson as Aeson
import Network.Flubber.Config
  ( PluginConfig(..)
  , pluginConfigArgs
  , pluginConfigPath
  )
import Network.Flubber.Monad (FlubberT, getPlugin, plugins)
import Network.Flubber.Plugins.Internal
  ( MonadPlugin(..)
  , Plugin(..)
  , pluginInitInfo
  , pluginRequests
  , Req
  , prismForResTo
  , unpackReq
  )
import qualified Network.Flubber.Plugins.Internal as Req
import Network.Flubber.Plugins.Types
  ( InitInfo
  , RequestBody(..)
  , ResponseBody(..)
  , initInfoProtocolVersion
  )
import Network.Flubber.Plugins.Worker (startWorkerThreads)
import Network.Flubber.Utils (conduitFromJSON, conduitToJSON, conduitXlatJSON)
import System.IO (Handle)
import System.Process.Typed
  ( Process
  , createPipe
  , getStdin
  , getStdout
  , proc
  , setStdin
  , setStdout
  , startProcess
  )

data PluginMisbehavior
  = BadInitInfo InitInfo
  | InvalidInitInfo String
  | InvalidResponseOrUpdate String
  | InvalidValue String
  | MissingInitInfo
  deriving (Eq, Show)

instance Exception PluginMisbehavior

spawnPlugin :: MonadIO m => PluginConfig -> m (Process Handle Handle ())
spawnPlugin p = startProcess config
  where config = setStdin createPipe .
                 setStdout createPipe $
                 proc (p^.pluginConfigPath) (p^.pluginConfigArgs)

parseInitInfo :: MonadThrow m => Maybe Value -> m InitInfo
parseInitInfo (Just v) = case fromJSON v of
                           Aeson.Success i -> pure i
                           Aeson.Error s -> throwM (InvalidInitInfo s)
parseInitInfo Nothing = throwM MissingInitInfo

checkInitInfo :: MonadThrow m => InitInfo -> m ()
checkInitInfo ii = if ok then pure () else throwM (BadInitInfo ii)
  where ok = (major == 0 && minor >= 1)
        (major, minor, _) = ii^.initInfoProtocolVersion

instance (MonadIO m, MonadThrow m) => MonadPlugin (FlubberT m) where
  startPlugin name conf = do
    -- Start the subprocess.
    process <- spawnPlugin conf
    -- Set up stdin.
    let stdin = conduitToJSON .| sinkHandle (getStdin process)
    -- Read the initInfo, and otherwise set up stdout.
    let stdoutValue = sourceHandle (getStdout process) .| conduitFromJSON InvalidValue
    initInfoValue <- liftIO $ runConduit (stdoutValue .| headC)
    initInfo <- parseInitInfo initInfoValue
    checkInitInfo initInfo
    let stdout = stdoutValue .| conduitXlatJSON InvalidResponseOrUpdate
    -- Spawn the worker thread.
    requests <- liftIO . atomically $ newTBMChan 10
    updates <- liftIO . atomically $ newTBMChan 10
    liftIO $ startWorkerThreads requests updates stdin stdout
    -- Store the plugin back.
    let plugin = MkPlugin initInfo process requests updates
    plugins.at name .= Just plugin
    -- Return.
    pure initInfo

  checkPlugin name = preuse (plugins.ix name.pluginInitInfo)

  request name req = do
    plugin <- getPlugin name
    var <- liftIO newEmptyMVar
    liftIO . atomically $ writeTBMChan (plugin^.pluginRequests) (unpackReq req, var)
    res <- liftIO $ readMVar var
    case res^?prismForResTo req of
      Just _ -> undefined
      Nothing -> undefined

  updatesFor name = undefined <$> getPlugin name
