module Network.Flubber.Config
  ( Config(..)
  , Plugin(..)
  , readConfig
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Toml (TomlCodec, (.=))
import qualified Toml

data Config = MkConfig
  { configPlugins :: Map Text Plugin
  } deriving (Eq, Show)

data Plugin = MkPlugin
  { pluginPath :: Text
  } deriving (Eq, Show)

configCodec :: TomlCodec Config
configCodec = (MkConfig . _)
  <$> Toml.table (Toml.list pluginCodec "") "plugins" .= configPlugins

pluginCodec :: TomlCodec Plugin
pluginCodec = MkPlugin
  <$> Toml.text "path" .= pluginPath

readConfig :: Text -> IO Config
readConfig = undefined
