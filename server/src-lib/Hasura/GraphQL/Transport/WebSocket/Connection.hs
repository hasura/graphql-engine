{-
  This module has WebSocket Connection related state and data types
-}

module Hasura.GraphQL.Transport.WebSocket.Connection where

import           Control.Concurrent                          (ThreadId)

import           Hasura.GraphQL.Transport.WebSocket.Protocol (OperationId)
import           Hasura.Prelude
import           Hasura.RQL.Types


import qualified Control.Concurrent.Async                    as A
import qualified Data.HashMap.Strict                         as Map
import qualified Network.HTTP.Types                          as HTTP
import qualified Network.WebSockets                          as WS


data ConnInitState
  = ConnInitState
  { _cisUserInfo   :: !UserInfo
  , _cisHeaders    :: ![HTTP.Header]
  , _cisRemoteConn :: !RemoteConnState
  } deriving (Show)

data WSConnState
  = CSNotInitialised
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
  , _wpsOperations      :: ![OperationId]
  }

instance Show (WebsocketProxyStateG a) where
  show _ =
    "WebsocketProxyStateG { (Async, Async), WebSocketConn }"

findRemoteName :: RemoteConnState -> RemoteSchemaName -> Maybe WebsocketProxyState
findRemoteName connMap rn = snd <$> find ((==) rn . fst) (Map.toList connMap)

findOperationId :: RemoteConnState -> OperationId -> Maybe WebsocketProxyState
findOperationId connMap opId =
  snd <$> find (elem opId . _wpsOperations . snd) (Map.toList connMap)

getStateData :: WSConnState -> Maybe ConnInitState
getStateData = \case
  CSInitialised d -> Just d
  _               -> Nothing
