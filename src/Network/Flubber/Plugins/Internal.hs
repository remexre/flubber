module Network.Flubber.Plugins.Internal
  ( MonadPlugin(..)
  , Plugin(..)
  , pluginInitInfo
  , pluginProcess
  , pluginRequests
  , pluginThread
  , pluginUpdates
  , Req(..)
  ) where

import Conduit (ConduitT)
import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import Control.Lens (makeLenses)
import Data.Text (Text)
import Network.Flubber.Config (PluginConfig)
import Network.Flubber.Plugins.Types
  ( InitInfo
  , Message
  , MessageID
  , MessageSend
  , RequestBody
  , ResponseBody
  , Room
  , RoomCreate
  , RoomID
  , Update
  )
import System.IO (Handle)
import System.Process.Typed (Process)

data Req res where
  MessageGetBefore :: MessageID -> Req ()
  MessageSend :: MessageSend -> Req Message
  RoomCreate :: RoomCreate -> Req Room
  RoomFind :: Text -> Req Room
  RoomJoin :: RoomID -> Req ()
  RoomLeave :: RoomID -> Req ()

class Monad m => MonadPlugin m where
  startPlugin :: Text -> PluginConfig -> m InitInfo
  checkPlugin :: Text -> m (Maybe InitInfo)
  request :: Text -> Req res -> m res
  updatesFor :: Text -> m (ConduitT () Update m ())

data Plugin = MkPlugin 
  { _pluginInitInfo :: InitInfo
  , _pluginProcess :: Process Handle Handle ()
  , _pluginRequests :: Chan (RequestBody, MVar ResponseBody)
  , _pluginThread :: ThreadId
  , _pluginUpdates :: Chan Update
  }

makeLenses ''Plugin

instance Show Plugin where
  show (MkPlugin initInfo _ _ thread _) = concat
    [ "MkPlugin {_pluginInitInfo = "
    , show initInfo
    , ", _pluginThread = "
    , show thread
    , "}"
    ]
