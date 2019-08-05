module Hasura.Server.Auth.JWT.Logging
  ( JwkRefreshLog (..)
  , JwkRefreshHttpError (..)
  , JwkLogNotice (..)
  )
  where

import           Data.Aeson

import           Hasura.HTTP
import           Hasura.Logging        (EngineLogType (..), LogLevel (..),
                                        ToEngineLog (..))
import           Hasura.Prelude
import           Hasura.Server.Logging ()
import           Hasura.Server.Utils   (httpExceptToJSON)

import qualified Data.Text             as T
import qualified Network.HTTP.Types    as HTTP


data JwkLogNotice
  = JLNInfo !Text
  | JLNError !Text
  deriving (Show)

data JwkRefreshLog
  = JwkRefreshLog
  { jrlLogLevel  :: !LogLevel
  , jrlNotice    :: !JwkLogNotice
  , jrlHttpError :: !(Maybe JwkRefreshHttpError)
  } deriving (Show)

data JwkRefreshHttpError
  = JwkRefreshHttpError
  { jrheStatus        :: !(Maybe HTTP.Status)
  , jrheUrl           :: !T.Text
  , jrheHttpException :: !(Maybe HttpException)
  , jrheResponse      :: !(Maybe T.Text)
  } deriving (Show)

instance ToJSON JwkRefreshHttpError where
  toJSON jhe =
    object [ "status_code" .= (HTTP.statusCode <$> jrheStatus jhe)
           , "url" .= jrheUrl jhe
           , "response" .= jrheResponse jhe
           , "http_exception" .= (httpExceptToJSON . unHttpException <$> jrheHttpException jhe)
           ]

instance ToJSON JwkRefreshLog where
  toJSON jrl = case jrlNotice jrl of
    JLNInfo info ->
      object [ "message" .= info
             , "http_error" .= (toJSON <$> jrlHttpError jrl)
             ]
    JLNError err ->
      object [ "error" .= err
             , "http_error" .= (toJSON <$> jrlHttpError jrl)
             ]

instance ToEngineLog JwkRefreshLog where
  toEngineLog jwkRefreshLog =
    (jrlLogLevel jwkRefreshLog, ELTJwkRefreshLog, toJSON jwkRefreshLog)
