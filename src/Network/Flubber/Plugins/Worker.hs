module Network.Flubber.Plugins.Worker
  ( startWorkerThreads
  ) where

import Conduit -- (ConduitT, (.|), mapM_C, runConduit)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Concurrent.STM.TBMChan (TBMChan, writeTBMChan)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, readTMVar, putTMVar)
import Control.Lens -- (makeLenses)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (atomically)
import Data.Conduit.TMChan (sourceTBMChan)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word32)
import Data.Void (Void)
import Network.Flubber.Plugins.Types
  ( Request(..)
  , RequestBody
  , responseBody
  , responseSequenceNumber
  , ResponseBody
  , ResponseOrUpdate(..)
  , Update
  )

data State = MkState
  { _nextSeqNum :: Word32
  , _vars :: Map Word32 (MVar ResponseBody)
  }

makeLenses ''State

defaultState :: State
defaultState = MkState 0 Map.empty

startWorkerThreads :: TBMChan (RequestBody, MVar ResponseBody) -> TBMChan Update
             -> ConduitT Request Void IO () -> ConduitT () ResponseOrUpdate IO () -> IO ()
startWorkerThreads requests updates stdin stdout = do
    stateVar <- atomically (newTMVar defaultState)
    _ <- forkIO (inputThread (sourceTBMChan requests) stdin stateVar)
    _ <- forkIO (outputThread updates stdout stateVar)
    pure ()

inputThread :: ConduitT () (RequestBody, MVar ResponseBody) IO () -> ConduitT Request Void IO ()
            -> TMVar State -> IO ()
inputThread requests stdin stateVar = runConduit (requests .| mapMC handle .| stdin)
  where handle (req, var) = do
         putStrLn ("req = " <> show req)
         req' <- atomically $ do
           state <- readTMVar stateVar
           let seqNum = state^.nextSeqNum
           putTMVar stateVar (MkState (seqNum + 1) (Map.insert seqNum var (state^.vars)))
           pure $ MkRequest seqNum req
         putStrLn ("req' = " <> show req')
         pure req'

outputThread :: TBMChan Update -> ConduitT () ResponseOrUpdate IO () -> TMVar State -> IO ()
outputThread updates stdout stateVar = runConduit (stdout .| mapM_C handle)
  where handle (Response res) = do
          let seqNum = res^.responseSequenceNumber
          maybeVar <- atomically $ do
            state <- readTMVar stateVar
            pure (state^?vars.ix seqNum)
          case maybeVar of
            Just var -> liftIO $ putMVar var (res^.responseBody)
            Nothing -> error "bruh" -- TODO: Log instead of crashing
        handle (Update update) = liftIO . atomically $ writeTBMChan updates update
