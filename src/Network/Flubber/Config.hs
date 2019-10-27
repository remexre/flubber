{-# LANGUAGE TemplateHaskell #-}

module Network.Flubber.Config
  ( Config(..)
  , PluginConfig(..)
  , configPlugins
  , configPort
  , pluginConfigArgs
  , pluginConfigPath
  , readConfig
  ) where

import Prelude hiding (readFile)

import Control.Lens (makeLenses)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (Options(..), defaultOptions, deriveFromJSON)
import Data.Aeson.Types (parseEither)
import Data.Char (toLower)
import Data.Either.Combinators (mapLeft)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text.IO (readFile)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Text.Toml (parseTomlDoc)

data PluginConfig = MkPluginConfig
  { _pluginConfigArgs :: [String]
  , _pluginConfigPath :: FilePath
  } deriving (Eq, Generic, Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = map toLower . drop 13} ''PluginConfig)
makeLenses ''PluginConfig

data Config = MkConfig
  { _configPlugins :: Map Text PluginConfig
  , _configPort :: Port
  } deriving (Eq, Generic, Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = map toLower . drop 7} ''Config)
makeLenses ''Config

readConfig :: FilePath -> IO (Either String Config)
readConfig path = runExceptT $ do
  contents <- liftIO $ readFile path
  table <- liftEither (mapLeft show (parseTomlDoc path contents))
  liftEither $ parseEither parseJSON (toJSON table)
