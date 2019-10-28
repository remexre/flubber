module Network.Flubber.Plugins
  ( MonadPlugin(..)
  , Plugin
  , pluginInitInfo
  , PluginMisbehavior(..)
  , Req(..)
  ) where

import Conduit ((.|), headC, runConduit, sinkHandle, sourceHandle)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, readMVar)
import Control.Lens ((.=), (^.), at, ix, preuse)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value, fromJSON)
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
  , Req(..)
  )
import Network.Flubber.Plugins.Types
  ( InitInfo
  , RequestBody(..)
  , ResponseBody(..)
  , initInfoProtocolVersion
  )
import Network.Flubber.Plugins.Worker (workerThread)
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

makeRequest :: Monad m => Req res -> (RequestBody, ResponseBody -> m res)
makeRequest = undefined

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
    requests <- liftIO newChan
    updates <- liftIO newChan
    thread <- liftIO $ forkIO (workerThread requests updates stdin stdout)
    -- Store the plugin back.
    let plugin = MkPlugin initInfo process requests thread updates
    plugins.at name .= Just plugin
    -- Return.
    pure initInfo

  checkPlugin name = preuse (plugins.ix name.pluginInitInfo)

  request name req = do
    plugin <- getPlugin name
    let (body, parse) = makeRequest req
    var <- liftIO newEmptyMVar
    liftIO $ writeChan (plugin^.pluginRequests) (body, var)
    res <- liftIO $ readMVar var
    parse res

  updatesFor name = undefined <$> getPlugin name
