module Hasura.Server.Auth.JWT.Logging
  ( JwkRefreshLog (..),
    JwkFetchError (..),
  )
where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Hasura.HTTP
import Hasura.Logging
  ( EngineLogType (..),
    Hasura,
    LogLevel (..),
    ToEngineLog (..),
  )
import Hasura.Prelude
import Network.HTTP.Types qualified as HTTP
import Network.URI (URI)

-- | Possible errors during fetching and parsing JWK
-- (the 'Text' type at the end is a friendly error message)
data JwkFetchError
  = -- | Exception while making the HTTP request
    JFEHttpException !HttpException !Text
  | -- | Non-2xx HTTP errors from the upstream server
    JFEHttpError !URI !HTTP.Status !BL.ByteString !Text
  | -- | Error parsing the JWK response itself
    JFEJwkParseError !Text !Text
  | -- | Error parsing the expiry of the JWK
    JFEExpiryParseError !(Maybe Text) Text
  deriving (Show)

instance ToJSON JwkFetchError where
  toJSON = \case
    JFEHttpException e _ ->
      object ["http_exception" .= e]
    JFEHttpError url status body _ ->
      object
        [ "status_code" .= HTTP.statusCode status,
          "url" .= tshow url,
          "response" .= bsToTxt (BL.toStrict body)
        ]
    JFEJwkParseError e msg ->
      object ["error" .= e, "message" .= msg]
    JFEExpiryParseError e msg ->
      object ["error" .= e, "message" .= msg]

data JwkRefreshLog = JwkRefreshLog
  { jrlLogLevel :: !LogLevel,
    jrlMessage :: !(Maybe Text),
    jrlError :: !(Maybe JwkFetchError)
  }
  deriving (Show)

instance ToJSON JwkRefreshLog where
  toJSON (JwkRefreshLog _ msg err) =
    object
      [ "message" .= msg,
        "error" .= err
      ]

instance ToEngineLog JwkRefreshLog Hasura where
  toEngineLog jwkRefreshLog =
    (jrlLogLevel jwkRefreshLog, ELTJwkRefreshLog, toJSON jwkRefreshLog)
