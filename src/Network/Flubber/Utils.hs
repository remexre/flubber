module Network.Flubber.Utils
  ( conduitFromJSON
  , conduitToJSON
  ) where

import Conduit (ConduitT, (.|), concatC, mapC)
import Data.Aeson.Types (FromJSON(..), ToJSON(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BS
import Data.Conduit.Combinators (mapAccumWhileM)
import Data.Functor (void)
import Data.JsonStream.Parser (ParseOutput(..), runParser', value)
import Katip (KatipContext, Severity(..), logTM, ls)

conduitFromJSON :: (FromJSON a, KatipContext m, Monad m) => ConduitT ByteString a m ()
conduitFromJSON = void (mapAccumWhileM loop defaultCont) .| concatC
  where defaultCont = runParser' value
        loop acc k = handle (k acc)
        handle (ParseYield v next) = handle next >>= \case
          Left k -> pure $ Right (k, [v])
          Right (k, vs) -> pure $ Right (k, v:vs)
        handle (ParseNeedData k) = pure $ Left k
        handle (ParseFailed err) = do
          $(logTM) ErrorS ("Parse failed: " <> ls err)
          pure $ Left defaultCont
        handle (ParseDone rest) = pure $ Left (defaultCont . (<> rest))

conduitToJSON :: (Monad m, ToJSON a) => ConduitT a ByteString m ()
conduitToJSON = mapC (BS.fromString . show . toJSON)
