module Main where

import Control.Lens ((^.), makeLenses)
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Katip (Severity(..), katipAddContext, logTM, sl)
import Network.Flubber.Config (PluginConfig, configPlugins, configPort, readConfig)
import Network.Flubber.Monad (FlubberT, runFlubberT)
import Network.Flubber.Plugins (MonadPlugin(..))
import Network.Wai.Handler.Warp (Port)
import Options.Applicative

data Args = MkArgs
  { _argsConfigFile :: FilePath
  , _argsPort :: Maybe Port
  } deriving (Eq, Show)

makeLenses ''Args

argParser :: Parser Args
argParser = MkArgs
  <$> strArgument
      ( help "The config file."
     <> metavar "CONFIG-FILE"
      )
  <*> (optional $ option auto $
        ( short 'p'
       <> long "port"
       <> help "The port to serve on."
       <> metavar "PORT"
        ))

parser :: ParserInfo Args
parser = info (argParser <**> helper) fullDesc

main :: IO ()
main = do
  args <- execParser parser
  config <- readConfig (args^.argsConfigFile) >>= \case
    Left err -> error err
    Right config -> pure config
  let plugins = config^.configPlugins
  let port = fromMaybe (config^.configPort) (args^.argsPort)
  runFlubberT (run plugins port)

run :: Map Text PluginConfig -> Port -> FlubberT IO ()
run plugins port = do
  forM_ (Map.toList plugins) $ \(name, plugin) -> do
    katipAddContext (sl "plugin" name) $ do
      spawnPlugin plugin
  katipAddContext (sl "port" port) $ do
    $(logTM) InfoS "Osu!game"
