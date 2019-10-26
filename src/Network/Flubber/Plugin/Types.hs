module Network.Flubber.Plugin.Types
  (
  ) where

import Data.Text (Text)

data Version = MkVersion
  { versionMajor :: !Word
  , versionMinor :: !Word
  , versionPatch :: !Word
  } deriving (Eq, Show)
