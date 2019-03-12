{-
  This module has WebSocket Connection related state and data types
-}

module Hasura.GraphQL.Transport.WebSocket.Connection where

import           Control.Concurrent                          (ThreadId)

import           Hasura.GraphQL.Transport.HTTP.Protocol      (OperationName)
import           Hasura.GraphQL.Transport.WebSocket.Protocol (ConnErrMsg,
                                                              OperationId)
import           Hasura.GraphQL.Transport.WebSocket.Server   (WSId)
import           Hasura.Prelude
import           Hasura.RQL.Types


--import qualified Control.Concurrent.Async                    as A
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.HashMap.Strict                         as Map
import qualified Hasura.Logging                              as L
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
  { _wpsRunClientThread :: !ThreadId
  , _wpsRemoteConn      :: !(Maybe WS.Connection)
  , _wpsOperations      :: ![GOperationId]
  }

instance Show (WebsocketProxyStateG a) where
  show (WebsocketProxyState thrdId wscon ops) =
    "WebsocketProxyState { "
    ++ "_wpsRunClientThread = " ++ show thrdId
    ++ ", _wpsRemoteConn = " ++ wsconD
    ++ ", _wpsOperations = " ++ show ops
    ++ " }"
    where
      wsconD = maybe "Nothing" (const "Just <WebsocketConn>") wscon

findRemoteName :: RemoteConnState -> RemoteSchemaName -> Maybe WebsocketProxyState
findRemoteName connMap rn = snd <$> find ((==) rn . fst) (Map.toList connMap)

findOperationId
  :: RemoteConnState -> GOperationId
  -> Maybe (RemoteSchemaName, WebsocketProxyState)
findOperationId connMap opId =
  find (elem opId . _wpsOperations . snd) $ Map.toList connMap

findWebsocketId
  :: RemoteConnState -> WSId
  -> Maybe (RemoteSchemaName, WebsocketProxyState)
findWebsocketId connMap wsId =
  find (elem wsId . map fst . _wpsOperations . snd) $ Map.toList connMap

getStateData :: WSConnState -> Maybe ConnInitState
getStateData = \case
  CSInitialised d -> Just d
  _               -> Nothing


data OpDetail
  = ODStarted
  | ODProtoErr !Text
  | ODQueryErr !QErr
  | ODCompleted
  | ODStopped
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 2
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''OpDetail)

data WSEvent
  = EAccepted
  | ERejected !QErr
  | EConnErr !ConnErrMsg
  | EOperation !OperationId !(Maybe OperationName) !OpDetail
  | EClosed
  deriving (Show, Eq)
$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase . drop 1
                   , J.sumEncoding = J.TaggedObject "type" "detail"
                   }
  ''WSEvent)

data WSLog
  = WSLog
  { _wslWebsocketId :: !WSId
  , _wslUser        :: !(Maybe UserVars)
  , _wslEvent       :: !WSEvent
  , _wslMsg         :: !(Maybe Text)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''WSLog)

instance L.ToEngineLog WSLog where
  toEngineLog wsLog =
    (L.LevelInfo, "ws-handler", J.toJSON wsLog)
