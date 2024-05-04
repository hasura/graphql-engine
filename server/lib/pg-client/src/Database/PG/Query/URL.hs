{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PG.Query.URL
  ( encodeURLPassword,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.URI (urlEncode)
import Prelude

-- | It is possible and common for postgres url's to have passwords with special
--   characters in them (ex AWS Secrets Manager passwords). Current URI parsing
--   libraries fail at parsing postgres uri's with special characters. Also note
--   that encoding the whole URI causes postgres to fail as well. This only
--   encodes the password when given a url.
encodeURLPassword :: Text -> Text
encodeURLPassword url =
  case Text.breakOnEnd "://" url of
    (_, "") -> url
    (scheme, urlWOScheme) -> case Text.breakOnEnd "@" urlWOScheme of
      ("", _) -> url
      (auth, rest) -> case Text.splitOn ":" $ Text.dropEnd 1 auth of
        [user] -> scheme <> user <> "@" <> rest
        (user : pass) -> scheme <> user <> ":" <> encode' pass <> "@" <> rest
        _ -> url
  where
    encode' arg =
      decodeUtf8 $ urlEncode True (encodeUtf8 $ Text.intercalate ":" arg)
