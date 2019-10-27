module Network.Flubber.Monad
  ( FlubberT(..)
  , FlubberConfig(..)
  , logContext
  , logEnv
  , logNamespace
  , runFlubberT
  ) where

import Control.Lens (makeLenses, over, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Katip
  ( ColorStrategy(..)
  , Katip(..)
  , KatipContext(..)
  , LogContexts
  , LogEnv
  , Namespace(..)
  , Verbosity(..)
  , closeScribes
  , defaultScribeSettings
  , initLogEnv
  , mkHandleScribe
  , registerScribe
  )
import System.IO (stderr)

data FlubberConfig = MkFlubberConfig
  { _logContext :: LogContexts
  , _logEnv :: LogEnv
  , _logNamespace :: Namespace
  }

makeLenses ''FlubberConfig

newtype FlubberT m a = MkFlubberT
  { unFlubberT :: ReaderT FlubberConfig m a
  } deriving (Functor, Applicative, Monad, MonadCatch, MonadIO, MonadMask, MonadReader FlubberConfig, MonadThrow)

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
    runReaderT (unFlubberT body) config
