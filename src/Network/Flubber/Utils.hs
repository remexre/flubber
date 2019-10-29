module Network.Flubber.Utils
  ( conduitFromJSON
  , conduitToJSON
  , conduitXlatJSON
  , jsonOptions
  ) where

import Conduit (ConduitT, (.|), concatC, mapC, mapMC)
import Control.Monad.Catch (Exception, MonadThrow(..))
import Data.Aeson (encode)
import Data.Aeson.Types
  ( FromJSON(..)
  , Options(..)
  , Result(..)
  , SumEncoding(..)
  , ToJSON(..)
  , defaultOptions
  , fromJSON
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.Conduit.Combinators (mapAccumWhileM)
import Data.Functor (void)
import Data.JsonStream.Parser (ParseOutput(..), runParser', value)

conduitFromJSON :: (Exception e, FromJSON a, MonadThrow m)
                => (String -> e) -> ConduitT ByteString a m ()
conduitFromJSON mkError = void (mapAccumWhileM loop defaultCont) .| concatC
  where defaultCont = runParser' value
        loop acc k = handle (k acc)
        handle (ParseYield v next) = handle next >>= \case
          Left k -> pure $ Right (k, [v])
          Right (k, vs) -> pure $ Right (k, v:vs)
        handle (ParseNeedData k) = pure $ Left k
        handle (ParseFailed err) = throwM (mkError err)
        handle (ParseDone rest) = pure $ Left (defaultCont . (<> rest))

conduitToJSON :: (Monad m, ToJSON a) => ConduitT a ByteString m ()
conduitToJSON = mapC (toStrict . encode)

conduitXlatJSON :: (Exception e, MonadThrow m, ToJSON a, FromJSON b)
                => (String -> e) -> ConduitT a b m ()
conduitXlatJSON mkError = mapMC (helper . toJSON)
  where helper v = case fromJSON v of
                     Success i -> pure i
                     Error s -> throwM (mkError s)

jsonOptions :: Int -> Options
jsonOptions n = defaultOptions{fieldLabelModifier = dropLH n, sumEncoding = se}
  where dropLH _ [] = []
        dropLH 0 "ID" = "id" -- dumb hack...
        dropLH 0 (h:t) = (toLower h):t
        dropLH i (_:t) = dropLH (i-1) t
        se = TaggedObject{tagFieldName = "type", contentsFieldName = "value"}
