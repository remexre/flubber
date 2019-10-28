module Network.Flubber.Plugins.Internal
  ( MonadPlugin(..)
  , Plugin(..)
  , pluginInitInfo
  , pluginProcess
  , pluginRequests
  , pluginUpdates
  , Req(..)
  , prismForResTo
  , unpackReq
  ) where

import Conduit (ConduitT)
import Control.Concurrent.STM.TBMChan (TBMChan)
import Control.Concurrent.MVar (MVar)
import Control.Lens (Prism', makeLenses)
import Data.Text (Text)
import Network.Flubber.Config (PluginConfig)
import Network.Flubber.Plugins.Types
  ( InitInfo
  , Message
  , MessageID
  , MessageSend
  , RequestBody
  , ResponseBody
  , _Success
  , _Message
  , _Room
  , _Error
  , Room
  , RoomCreate
  , RoomID
  , Update
  )
import qualified Network.Flubber.Plugins.Types as Ty
import System.IO (Handle)
import System.Process.Typed (Process)

data Req res where
  MessageGetBefore :: MessageID -> Req ()
  MessageSend :: MessageSend -> Req MessageID
  RoomCreate :: RoomCreate -> Req RoomID
  RoomFind :: Text -> Req RoomID
  RoomJoin :: RoomID -> Req ()
  RoomLeave :: RoomID -> Req ()

prismForResTo :: Req res -> Prism' ResponseBody res
prismForResTo (MessageGetBefore id) = error "TODO MessageGetBefore"
prismForResTo (MessageSend msg) = error "TODO MessageSend"
prismForResTo (RoomCreate room) = error "TODO RoomCreate"
prismForResTo (RoomFind name) = _Room
prismForResTo (RoomJoin id) = error "TODO RoomJoin"
prismForResTo (RoomLeave id) = error "TODO RoomLeave"

unpackReq :: Req res -> RequestBody
unpackReq (MessageGetBefore id) = error "TODO MessageGetBefore"
unpackReq (MessageSend msg) = error "TODO MessageSend"
unpackReq (RoomCreate room) = error "TODO RoomCreate"
unpackReq (RoomFind name) = Ty.RoomFind name
unpackReq (RoomJoin id) = error "TODO RoomJoin"
unpackReq (RoomLeave id) = error "TODO RoomLeave"

class Monad m => MonadPlugin m where
  startPlugin :: Text -> PluginConfig -> m InitInfo
  checkPlugin :: Text -> m (Maybe InitInfo)
  request :: Text -> Req res -> m res
  updatesFor :: Text -> m (ConduitT () Update m ())

data Plugin = MkPlugin 
  { _pluginInitInfo :: InitInfo
  , _pluginProcess :: Process Handle Handle ()
  , _pluginRequests :: TBMChan (RequestBody, MVar ResponseBody)
  , _pluginUpdates :: TBMChan Update
  }

makeLenses ''Plugin

instance Show Plugin where
  show (MkPlugin initInfo _ _ _) = concat
    [ "MkPlugin {_pluginInitInfo = "
    , show initInfo
    , "}"
    ]
