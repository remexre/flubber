module Main where

import Control.Concurrent (forkIO)
import Control.Lens ((^.), makeLenses)
import Data.Maybe (fromMaybe)
import Network.Flubber.Config (configPlugins, configPort, readConfig)
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
  print (port, plugins)
