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


import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.Time.Clock                             as TC
import qualified Hasura.Logging                              as L
import qualified Network.HTTP.Types                          as HTTP
import qualified Network.WebSockets                          as WS
import qualified StmContainers.Map                           as STMMap

import qualified Hasura.GraphQL.Execute.LiveQuery            as LQ

-- uniquely identifies an operation
type GOperationId = (WSId, OperationId)

data ConnInitState
  = ConnInitState
  { _cisUserInfo  :: !UserInfo
  , _cisJwtExpiry :: !(Maybe TC.UTCTime)
  -- headers from the client (in conn params) to forward to the remote schema
  , _cisHeaders   :: ![HTTP.Header]
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

type OperationMap
  = STMMap.Map OperationId SubscriptionOperation

data SubscriptionOperation
  = SOHasura !(LQ.LiveQueryId, Maybe OperationName)
  | SORemote !RemoteOperation

data RemoteOperation
  = RemoteOperation
  { _ropRunClientThread :: !ThreadId
  , _ropRemoteConn      :: !WS.Connection
  , _ropRemoteName      :: !RemoteSchemaName
  , _ropClientWsId      :: !WSId
  }

instance Show RemoteOperation where
  show (RemoteOperation thrdId _ rn wsId) =
    "WebsocketProxyState { "
    ++ "_ropRunClientThread = " ++ show thrdId
    ++ ", _ropRemoteConn = <WebsocketConn>"
    ++ ", _ropRemoteName = " ++ show rn
    ++ ", _ropClientWsId = " ++ show wsId
    ++ " }"


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
  , _wslJwtExpiry   :: !(Maybe TC.UTCTime)
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''WSLog)

instance L.ToEngineLog WSLog where
  toEngineLog wsLog =
    (L.LevelInfo, "ws-handler", J.toJSON wsLog)
