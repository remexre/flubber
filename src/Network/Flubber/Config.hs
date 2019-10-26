{-# LANGUAGE TemplateHaskell #-}

module Network.Flubber.Config
  ( Config(..)
  , Plugin(..)
  , configPlugins
  , configPort
  , pluginArgs
  , pluginPath
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

data Plugin = MkPlugin
  { _pluginArgs :: [Text]
  , _pluginPath :: Text
  } deriving (Eq, Generic, Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = map toLower . drop 7} ''Plugin)
makeLenses ''Plugin

data Config = MkConfig
  { _configPlugins :: Map Text Plugin
  , _configPort :: Port
  } deriving (Eq, Generic, Show)

$(deriveFromJSON defaultOptions{fieldLabelModifier = map toLower . drop 7} ''Config)
makeLenses ''Config

readConfig :: FilePath -> IO (Either String Config)
readConfig path = runExceptT $ do
  contents <- liftIO $ readFile path
  table <- liftEither (mapLeft show (parseTomlDoc path contents))
  liftEither $ parseEither parseJSON (toJSON table)
