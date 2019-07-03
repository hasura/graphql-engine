module Hasura.Server.Auth.JWT.Logging
  ( JwkRefreshLog (..)
  , JwkRefreshHttpError (..)
  , mkJwkRefreshLog
  )
  where

import           Data.Aeson

import           Hasura.HTTP
import           Hasura.Logging        (EngineLogType (..), LogLevel (..),
                                        ToEngineLog (..))
import           Hasura.Prelude
import           Hasura.Server.Logging ()

import qualified Data.Text             as T
import qualified Network.HTTP.Types    as HTTP


data JwkRefreshLog
  = JwkRefreshLog
  { jrlLogLevel  :: !LogLevel
  , jrlError     :: !T.Text
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
           , "http_exception" .= (toJSON <$> jrheHttpException jhe)
           ]

instance ToJSON JwkRefreshLog where
  toJSON jrl =
    object [ "error" .= jrlError jrl
           , "http_error" .= (toJSON <$> jrlHttpError jrl)
           ]

instance ToEngineLog JwkRefreshLog where
  toEngineLog jwkRefreshLog =
    (jrlLogLevel jwkRefreshLog, ELTJwkRefreshLog, toJSON jwkRefreshLog)

mkJwkRefreshLog :: LogLevel -> T.Text -> Maybe JwkRefreshHttpError -> JwkRefreshLog
mkJwkRefreshLog = JwkRefreshLog
