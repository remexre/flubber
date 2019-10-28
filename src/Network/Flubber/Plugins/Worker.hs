module Network.Flubber.Plugins.Worker
  ( workerThread
  ) where

import Conduit -- (ConduitT, (.|), mapM_C, runConduit)
import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Lens -- (makeLenses)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word32)
import Network.Flubber.Plugins.Types
  ( Request
  , RequestBody
  , Response
  , responseBody
  , responseSequenceNumber
  , ResponseBody
  , Update
  )

data State = MkState
  { _nextSeqNum :: Word32
  , _vars :: Map Word32 (MVar ResponseBody)
  }

makeLenses ''State

defaultState :: State
defaultState = MkState 0 Map.empty

type M = StateT State IO

workerThread :: Chan (RequestBody, MVar ResponseBody) -> Chan Update
             -> ConduitT Request () IO () -> ConduitT () (Either Response Update) IO () -> IO ()
workerThread requests updates stdin stdout = evalStateT body defaultState
  where addState c = stateC (\s -> (,s) <$> c)
        body = workerThread' requests updates (addState stdin) (addState stdout)

workerThread' :: Chan (RequestBody, MVar ResponseBody) -> Chan Update
              -> ConduitT Request () M () -> ConduitT () (Either Response Update) M () -> M ()
workerThread' requests updates stdin stdout = do
  let output = stdout .| mapM_C (handleOutput updates)
  runConduit output

handleOutput :: Chan Update -> Either Response Update -> M ()
handleOutput _ (Left res) = do
  let seqNum = res^.responseSequenceNumber
  maybeVar <- preuse (vars.ix seqNum)
  case maybeVar of
    Just var -> liftIO $ putMVar var (res^.responseBody)
    Nothing -> error "bruh" -- TODO: Log
handleOutput updates (Right update) = liftIO $ writeChan updates update
