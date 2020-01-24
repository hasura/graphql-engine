module Hasura.Server.Auth.JWT.Logging
  ( JwkRefreshLog (..)
  , JwkFetchError (..)
  )
  where

import           Data.Aeson
import           Network.URI           (URI)

import           Hasura.HTTP
import           Hasura.Logging        (EngineLogType (..), Hasura, InternalLogTypes (..),
                                        LogLevel (..), ToEngineLog (..))
import           Hasura.Prelude
import           Hasura.Server.Logging ()

import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import qualified Network.HTTP.Types    as HTTP


-- | Possible errors during fetching and parsing JWK
data JwkFetchError
  = JFEHttpException !HttpException !Text
  -- ^ Exception while making the HTTP request. Text is the error message
  | JFEHttpError !URI !HTTP.Status !BL.ByteString !Text
  -- ^ Non-2xx HTTP errors from the upstream server. Url, status, body and error message
  | JFEJwkParseError !Text !Text
  -- ^ Error parsing the JWK response itself. Text: Actual parse error and friendly error message
  | JFEExpiryParseError !Text !Text
  -- ^ Error parsing the expiry of the JWK. Text: Actual parse error and friendly error message
  deriving (Show)

instance ToJSON JwkFetchError where
  toJSON = \case
    JFEHttpException e _ ->
      object [ "http_exception" .= e ]

    JFEHttpError url status body _ ->
      object [ "status_code" .= HTTP.statusCode status
             , "url" .= T.pack (show url)
             , "response" .= bsToTxt (BL.toStrict body)
             ]

    JFEJwkParseError e msg ->
      object [ "parse_error" .= e, "message" .= msg ]

    JFEExpiryParseError e msg ->
      object [ "parse_error" .= e, "message" .= msg ]


-- data JwkRefreshHttpError
--   = JwkRefreshHttpError
--   { jrheStatus        :: !(Maybe HTTP.Status)
--   , jrheUrl           :: !T.Text
--   , jrheHttpException :: !(Maybe HttpException)
--   , jrheResponse      :: !(Maybe T.Text)
--   } deriving (Show)

-- instance ToJSON JwkRefreshHttpError where
--   toJSON jhe =
--     object [ "status_code" .= (HTTP.statusCode <$> jrheStatus jhe)
--            , "url" .= jrheUrl jhe
--            , "response" .= jrheResponse jhe
--            , "http_exception" .= (httpExceptToJSON . unHttpException <$> jrheHttpException jhe)
--            ]

data JwkRefreshLog
  = JwkRefreshLog
  { jrlLogLevel :: !LogLevel
  , jrlMessage  :: !(Maybe Text)
  , jrlError    :: !(Maybe JwkFetchError)
  } deriving (Show)

instance ToJSON JwkRefreshLog where
  toJSON (JwkRefreshLog _ msg err) =
      object [ "message" .= msg
             , "error" .= err
             ]

instance ToEngineLog JwkRefreshLog Hasura where
  toEngineLog jwkRefreshLog =
    (jrlLogLevel jwkRefreshLog, ELTInternal ILTJwkRefreshLog, toJSON jwkRefreshLog)
