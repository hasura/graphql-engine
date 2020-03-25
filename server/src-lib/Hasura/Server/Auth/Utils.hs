module Hasura.Server.Auth.Utils
  ( timeFromCacheControlHeader
  , timeFromExpiresHeader
  , toUTCTime
  , toNominalDiffTime
  ) where

import           Data.Parser.CacheControl (parseMaxAge)
import           Data.Time.Clock
import           Data.Time.Format         (defaultTimeLocale, parseTimeM)

import qualified Data.String.Conversions  as CS
import qualified Data.Text                as T

import           Hasura.Prelude


data ExpiryTime = AbsoluteTime UTCTime
                | RelativeTime NominalDiffTime


-- | Extracts a relative expiry time from the CacheControl header.
timeFromCacheControlHeader
  :: (Monad m)
  => Maybe T.Text         -- ^ the header
  -> (T.Text -> m ())     -- ^ what should be done if parsing fails
  -> m (Maybe ExpiryTime)
timeFromCacheControlHeader header onError =
  onMaybe header $ \h -> case parseMaxAge h of
    Left e       -> onError (T.pack e) >> return Nothing
    Right maxAge -> return $ Just $ RelativeTime $ fromInteger maxAge

-- | Extracts an absolute expiry time from a the Expires header.
timeFromExpiresHeader
  :: (Monad m)
  => Maybe T.Text         -- ^ the header
  -> (T.Text -> m ())     -- ^ what should be done if parsing fails
  -> m (Maybe ExpiryTime)
timeFromExpiresHeader header onError =
  onMaybe header $ \h -> case parseTimeM True defaultTimeLocale timeFmt $ CS.cs h of
    Nothing      -> onError "Value of Expires header is not a valid timestamp" >> return Nothing
    Just expTime -> return $ Just $ AbsoluteTime expTime
  where timeFmt = "%a, %d %b %Y %T GMT"


toUTCTime :: (MonadIO m) => ExpiryTime -> m UTCTime
toUTCTime (AbsoluteTime t) = return t
toUTCTime (RelativeTime d) = addUTCTime d <$> liftIO getCurrentTime

toNominalDiffTime :: (MonadIO m) => ExpiryTime -> m NominalDiffTime
toNominalDiffTime (RelativeTime d) = return d
toNominalDiffTime (AbsoluteTime t) = diffUTCTime t <$> liftIO getCurrentTime
