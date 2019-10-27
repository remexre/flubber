module Network.Flubber.Plugins.Types
  ( InitInfo(..)
  , initInfoPluginName
  , initInfoPluginVersion
  , initInfoProtocolVersion
  , MessageID(..)
  , Message(..)
  , messageID
  , messageSender
  , messageRecipient
  , messageAttachments
  , messageContent
  , messageCreateTime
  , messageEditTime
  , messageExtra
  , MessageAttachment(..)
  , messageAttachmentMIME
  , messageAttachmentData
  , MessageContent(..)
  , _Bold
  , _Concat
  , _Crossout
  , _Emote
  , _Italic
  , _MessageLink
  , _RoomLink
  , _Text
  , _UrlLink
  , _Underline
  , _UserLink
  , MessageSend(..)
  , messageSendRecipient
  , messageSendAttachments
  , messageSendContent
  , messageSendExtra
  , RoomID(..)
  , Room(..)
  , roomID
  , roomParent
  , roomName
  , roomSendable
  , RoomCreate(..)
  , roomCreateParent
  , roomCreateName
  , roomCreateSendable
  , UserID(..)
  , Request(..)
  , requestSequenceNumber
  , requestBody
  , RequestBody(..)
  , _MessageGetBefore
  , _MessageSend
  , _RoomCreate
  , _RoomFind
  , _RoomJoin
  , _RoomLeave
  , Response(..)
  , responseSequenceNumber
  , responseBody
  , ResponseBody
  , _Success
  , _Message
  , _Room
  , _Error
  , ResponseError(..)
  , responseErrorMessage
  , responseErrorRetry
  ) where

import Control.Lens (makeLenses, makePrisms, makeWrapped)
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word32)

data InitInfo = MkInitInfo
  { _initInfoPluginName :: Text
  , _initInfoPluginVersion :: (Word32, Word32, Word32)
  , _initInfoProtocolVersion :: (Word32, Word32, Word32)
  } deriving (Eq, Show)

newtype MessageID = MkMessageID Text
  deriving (Eq, Ord, Show)

data Message = MkMessage
  { _messageID :: MessageID
  , _messageSender :: UserID
  , _messageRecipient :: Either RoomID UserID
  , _messageAttachments :: Seq MessageAttachment
  , _messageContent :: MessageContent
  , _messageCreateTime :: UTCTime
  , _messageEditTime :: UTCTime
  , _messageExtra :: Value
  } deriving (Eq, Show)

data MessageAttachment = MkMessageAttachment
  { _messageAttachmentMIME :: Maybe Text
  , _messageAttachmentData :: ByteString
  } deriving (Eq, Show)

data MessageContent
  = Bold MessageContent
  | Concat (Seq MessageContent)
  | Crossout MessageContent
  | Emote Text
  | Italic MessageContent
  | MessageLink MessageID
  | RoomLink RoomID
  | Text Text
  | UrlLink Text
  | Underline MessageContent
  | UserLink UserID
  deriving (Eq, Show)

data MessageSend = MkMessageSend
  { _messageSendRecipient :: Either RoomID UserID
  , _messageSendAttachments :: Seq MessageAttachment
  , _messageSendContent :: MessageContent
  , _messageSendExtra :: Value
  } deriving (Eq, Show)

newtype RoomID = MkRoomID Text
  deriving (Eq, Ord, Show)

data Room = MkRoom
  { _roomID :: RoomID
  , _roomParent :: Maybe RoomID
  , _roomName :: Text
  , _roomSendable :: Bool
  } deriving (Eq, Show)

data RoomCreate = MkRoomCreate
  { _roomCreateParent :: Maybe RoomID
  , _roomCreateName :: Text
  , _roomCreateSendable :: Bool
  } deriving (Eq, Show)

newtype UserID = MkUserID Text
  deriving (Eq, Ord, Show)

data Request = MkRequest
  { _requestSequenceNumber :: Word32
  , _requestBody :: RequestBody
  } deriving (Eq, Show)

data RequestBody
  = MessageGetBefore MessageID
  | MessageSend MessageSend
  | RoomCreate RoomCreate
  | RoomFind Text
  | RoomJoin RoomID
  | RoomLeave RoomID
  deriving (Eq, Show)

data Response = MkResponse
  { _responseSequenceNumber :: Word32
  , _responseBody :: ResponseBody
  } deriving (Eq, Show)

data ResponseBody
  = Success
  | Message MessageID
  | Room RoomID
  | Error ResponseError
  deriving (Eq, Show)

data ResponseError = MkResponseError
  { _responseErrorMessage :: Text
  , _responseErrorRetry :: Bool
  } deriving (Eq, Show)

makeLenses ''InitInfo
makeWrapped ''MessageID
makeLenses ''Message
makeLenses ''MessageAttachment
makePrisms ''MessageContent
makeLenses ''MessageSend
makeWrapped ''RoomID
makeLenses ''Room
makeLenses ''RoomCreate
makeWrapped ''UserID
makeLenses ''Request
makePrisms ''RequestBody
makeLenses ''Response
makePrisms ''ResponseBody
makeLenses ''ResponseError
