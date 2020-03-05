{-|
This module holds functions and data types used for logging at the GraphQL
layer. In contrast with, logging at the HTTP server layer.
-}

module Hasura.GraphQL.Logging
  ( QueryLog(..)
  , WebSocketLog(..)
  , WSLog(..)
  , WSLogInfo(..)
  , mkWsLog
  ) where

import qualified Data.Aeson                               as J
import qualified Data.Aeson.Casing                        as J
import qualified Data.Aeson.TH                            as J
import qualified Language.GraphQL.Draft.Syntax            as G
import qualified Network.HTTP.Types                       as H

import           Hasura.GraphQL.Transport.HTTP.Protocol   (GQLReqUnparsed)
import           Hasura.GraphQL.Transport.WebSocket.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils                      (RequestId)

import qualified Hasura.GraphQL.Execute.Query             as EQ
import qualified Hasura.Logging                           as L


-- | A GraphQL query, optionally generated SQL, and the request id makes up the
-- | 'QueryLog'
data QueryLog
  = QueryLog
  { _qlQuery        :: !GQLReqUnparsed
  , _qlGeneratedSql :: !(Maybe EQ.GeneratedSqlMap)
  , _qlRequestId    :: !RequestId
  }

instance J.ToJSON QueryLog where
  toJSON (QueryLog q sql reqId) =
    J.object [ "query" J..= q
             , "generated_sql" J..= (encodeSql <$> sql)
             , "request_id" J..= reqId
             ]

instance L.ToEngineLog QueryLog L.Hasura where
  toEngineLog ql = (L.LevelInfo, L.ELTQueryLog, J.toJSON ql)

-- | Helper function to convert the list of alias to generated SQL into a
-- | key-value map to be printed as JSON
encodeSql :: EQ.GeneratedSqlMap -> J.Value
encodeSql sql =
  jValFromAssocList $ map (\(a, q) -> (alName a, fmap J.toJSON q)) sql
  where
    alName = G.unName . G.unAlias
    jValFromAssocList xs = J.object $ map (uncurry (J..=)) xs

data WSLogInfo
  = WSLogInfo
  { _wsliUserVars       :: !(Maybe UserVars)
  , _wsliConnectionInfo :: !WSConnInfo
  , _wsliEvent          :: !WSEvent
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 5 J.snakeCase) ''WSLogInfo)

data WSLog
  = WSLog
  { _wslLogLevel :: !L.LogLevel
  , _wslInfo     :: !WSLogInfo
  }

instance L.ToEngineLog WSLog L.Hasura where
  toEngineLog (WSLog logLevel wsLog) =
    (logLevel, L.ELTWebsocketLog, J.toJSON wsLog)

class (Monad m) => WebSocketLog m where
  logWebSocket
    :: L.Logger L.Hasura
    -- ^ the logger
    -> L.LogLevel
    -- ^ log level
    -> Maybe UserVars
    -- ^ user vars may or may not be present (error can happen during user resolution)
    -> WSConnInfo
    -- ^ websocket connection info
    -> WSEvent
    -- ^ websocket event
    -> [H.Header]
    -- ^ list of request headers
    -> m ()


  logWebSocketError
    :: L.Logger L.Hasura
    -- ^ the logger
    -> Maybe UserVars
    -- ^ user vars may or may not be present (error can happen during user resolution)
    -> WSConnInfo
    -- ^ websocket connection info
    -> WSEvent
    -- ^ websocket event
    -> [H.Header]
    -- ^ list of request headers
    -> m ()
  logWebSocketError logger = logWebSocket logger L.LevelError

  logWebSocketSuccess
    :: L.Logger L.Hasura
    -- ^ the logger
    -> Maybe UserVars
    -- ^ user vars may or may not be present (error can happen during user resolution)
    -> WSConnInfo
    -- ^ websocket connection info
    -> WSEvent
    -- ^ websocket event
    -> [H.Header]
    -- ^ list of request headers
    -> m ()
  logWebSocketSuccess logger = logWebSocket logger L.LevelInfo

mkWsLog :: L.LogLevel -> Maybe UserVars -> WSConnInfo -> WSEvent -> WSLog
mkWsLog level uv ci ev = WSLog level $ WSLogInfo uv ci ev
