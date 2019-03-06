{-
  This module has WebSocket Connection related state and data types
-}

module Hasura.GraphQL.Transport.WebSocket.Connection where

import           Control.Concurrent                          (ThreadId)

import           Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import           Hasura.GraphQL.Transport.WebSocket.Server   (WSId)
import           Hasura.Prelude
import           Hasura.RQL.Types


import qualified Control.Concurrent.Async                    as A
import qualified Data.HashMap.Strict                         as Map
import qualified Network.HTTP.Types                          as HTTP
import qualified Network.WebSockets                          as WS


-- uniquely identifies an operation
type GOperationId = (WSId, OperationId)

data ConnInitState
  = ConnInitState
  { _cisUserInfo   :: !UserInfo
  -- headers from the client (in conn params) to forward to the remote schema
  , _cisHeaders    :: ![HTTP.Header]
  , _cisRemoteConn :: !RemoteConnState
  } deriving (Show)

newtype WsHeaders
  = WsHeaders { unWsHeaders :: [HTTP.Header] }
  deriving (Show, Eq)

data WSConnState
  -- headers from the client for websockets
  = CSNotInitialised !WsHeaders
  | CSInitError Text
  | CSInitialised ConnInitState
  deriving (Show)

type RemoteConnState
  = Map.HashMap RemoteSchemaName WebsocketProxyState

type WebsocketProxyState = WebsocketProxyStateG ()

data WebsocketProxyStateG a
  = WebsocketProxyState
  { _wpsRemoteRcvr      :: !(A.Async a)
  , _wpsRunClientThread :: !(Maybe ThreadId)
  , _wpsRemoteConn      :: !WS.Connection
  , _wpsOperations      :: ![GOperationId]
  }

instance Show (WebsocketProxyStateG a) where
  show _ =
    "WebsocketProxyStateG { (Async, Async), WebSocketConn }"

findRemoteName :: RemoteConnState -> RemoteSchemaName -> Maybe WebsocketProxyState
findRemoteName connMap rn = snd <$> find ((==) rn . fst) (Map.toList connMap)

findOperationId :: RemoteConnState -> OperationId -> Maybe WebsocketProxyState
findOperationId connMap opId =
  snd <$> find (elem opId . map snd . _wpsOperations . snd) (Map.toList connMap)

findWebsocketId :: RemoteConnState -> WSId -> Maybe WebsocketProxyState
findWebsocketId connMap wsId =
  snd <$> find (elem wsId . map fst . _wpsOperations . snd) (Map.toList connMap)

getStateData :: WSConnState -> Maybe ConnInitState
getStateData = \case
  CSInitialised d -> Just d
  _               -> Nothing
