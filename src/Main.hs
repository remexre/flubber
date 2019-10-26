module Main where

import Control.Concurrent (forkIO)
import Network.Flubber.Config (readConfig)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative

data Args = MkArgs
  { configFile :: Text
  , port :: Port
  } deriving (Eq, Show)

argParser :: Parser Args
argParser = MkArgs
  <$> strArgument
      ( help "The config file."
     <> metavar "CONFIG-FILE"
      )
  <*> option auto
      ( short 'p'
     <> long "port"
     <> help "The port to serve on."
     <> metavar "PORT"
      )

parser :: ParserInfo Args
parser = info (argParser <**> helper)
  ( fullDesc
 <> progDesc "progDesc"
 <> header "flubber"
  )

main :: IO ()
main = do
  args <- execParser parser
  print args
  config <- readConfig (configFile args)
  print config
