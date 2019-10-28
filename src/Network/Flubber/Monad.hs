module Network.Flubber.Monad
  ( FlubberT(..)
  , runFlubberT
  , FlubberConfig(..)
  , logContext
  , logEnv
  , logNamespace
  , FlubberState(..)
  , plugins
  , NoSuchPlugin(..)
  , getPlugin
  ) where

import Control.Lens (makeLenses, ix, over, preuse, use, view)
import Control.Monad.Catch (Exception, MonadCatch, MonadMask, MonadThrow(..), bracket)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Katip
  ( ColorStrategy(..)
  , Katip(..)
  , KatipContext(..)
  , LogContexts
  , LogEnv
  , Namespace(..)
  , Severity(..)
  , Verbosity(..)
  , closeScribes
  , defaultScribeSettings
  , initLogEnv
  , logTM
  , mkHandleScribe
  , registerScribe
  , showLS
  )
import Network.Flubber.Plugins.Internal (Plugin)
import System.IO (stderr)

data FlubberConfig = MkFlubberConfig
  { _logContext :: LogContexts
  , _logEnv :: LogEnv
  , _logNamespace :: Namespace
  }

makeLenses ''FlubberConfig

data FlubberState = MkFlubberState
  { _plugins :: Map Text Plugin
  } deriving Show

makeLenses ''FlubberState

newtype FlubberT m a = MkFlubberT
  { unFlubberT :: ReaderT FlubberConfig (StateT FlubberState m) a
  } deriving ( Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask
             , MonadReader FlubberConfig, MonadState FlubberState, MonadThrow
             )

instance MonadIO m => Katip (FlubberT m) where
  getLogEnv = view logEnv
  localLogEnv f (MkFlubberT m) = MkFlubberT (local (over logEnv f) m)

instance MonadIO m => KatipContext (FlubberT m) where
  getKatipContext = view logContext
  getKatipNamespace = view logNamespace
  localKatipContext f (MkFlubberT m) = MkFlubberT (local (over logContext f) m)
  localKatipNamespace f (MkFlubberT m) = MkFlubberT (local (over logNamespace f) m)

runFlubberT :: (MonadIO m, MonadMask m) => FlubberT m a -> m a
runFlubberT body = do
  let register = liftIO $ do
        le <- initLogEnv mempty "production"
        stderrScribe <- mkHandleScribe ColorIfTerminal stderr (const $ pure True) V3
        registerScribe "stderr" stderrScribe defaultScribeSettings le
  bracket register (liftIO . closeScribes) $ \le -> do
    let config = MkFlubberConfig
          { _logContext = mempty
          , _logEnv = le
          , _logNamespace = "flubber"
          }
    evalStateT (runReaderT (unFlubberT body) config) (MkFlubberState Map.empty)

newtype NoSuchPlugin
  = MkNoSuchPlugin Text
  deriving (Eq, Show)

instance Exception NoSuchPlugin

getPlugin :: (KatipContext m, MonadState FlubberState m, MonadThrow m) => Text -> m Plugin
getPlugin name = do
  maybePlugin <- preuse (plugins.ix name)
  case maybePlugin of
    Just plugin -> pure plugin
    Nothing -> do
      allPs <- showLS . Map.keys <$> use plugins
      let name' = showLS name
      $(logTM) ErrorS ("Couldn't find " <> name' <> "; the following were available: " <> allPs)
      throwM (MkNoSuchPlugin name)
