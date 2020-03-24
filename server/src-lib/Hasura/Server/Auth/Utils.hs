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


timeFromCacheControlHeader
  :: (Monad m)
  => Maybe T.Text
  -> (T.Text -> m ())
  -> m (Maybe ExpiryTime)
timeFromCacheControlHeader header onError =
  onMaybe header $ \h -> case parseMaxAge h of
    Left e       -> onError (T.pack e) >> return Nothing
    Right maxAge -> return $ Just $ RelativeTime $ fromInteger maxAge

timeFromExpiresHeader
  :: (Monad m)
  => Maybe T.Text
  -> (T.Text -> m ())
  -> m (Maybe ExpiryTime)
timeFromExpiresHeader header onError =
  onMaybe header $ \h -> case parseTimeM True defaultTimeLocale timeFmt $ CS.cs h of
    Left e        -> onError (T.pack e) >> return Nothing
    Right expTime -> return $ Just $ AbsoluteTime expTime
  where timeFmt = "%a, %d %b %Y %T GMT"


toUTCTime :: (MonadIO m) => ExpiryTime -> m UTCTime
toUTCTime (AbsoluteTime t) = return t
toUTCTime (RelativeTime d) = addUTCTime d <$> liftIO getCurrentTime

toNominalDiffTime :: (MonadIO m) => ExpiryTime -> m NominalDiffTime
toNominalDiffTime (RelativeTime d) = return d
toNominalDiffTime (AbsoluteTime t) = diffUTCTime t <$> liftIO getCurrentTime


onMaybe :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
onMaybe m action = maybe (return Nothing) action m
