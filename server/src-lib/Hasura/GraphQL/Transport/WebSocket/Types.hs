module Hasura.GraphQL.Transport.WebSocket.Types where

import           Hasura.Prelude

import qualified Control.Concurrent.STM                      as STM
import qualified Data.Time.Clock                             as TC
import qualified Network.HTTP.Client                         as H
import qualified Network.HTTP.Types                          as H
import qualified Network.Wai.Extended                        as Wai
import qualified StmContainers.Map                           as STMMap

import qualified Hasura.GraphQL.Execute                      as E
import qualified Hasura.GraphQL.Execute.LiveQuery.State      as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import qualified Hasura.Logging                              as L

import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.Instances          ()
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import           Hasura.RQL.Types
import           Hasura.Server.Cors
import           Hasura.Server.Init.Config                   (KeepAliveDelay (..))
import           Hasura.Server.Metrics                       (ServerMetrics (..))
import           Hasura.Session

newtype WsHeaders
  = WsHeaders { unWsHeaders :: [H.Header] }
  deriving (Show, Eq)

data ErrRespType
  = ERTLegacy
  | ERTGraphqlCompliant
  deriving (Show)

data WSConnState
  = CSNotInitialised !WsHeaders !Wai.IpAddress
  -- ^ headers and IP address from the client for websockets
  | CSInitError !Text
  | CSInitialised !WsClientState
  deriving (Show)

data WsClientState
  = WsClientState
  { wscsUserInfo     :: !UserInfo
  -- ^ the 'UserInfo' required to execute the GraphQL query
  , wscsTokenExpTime :: !(Maybe TC.UTCTime)
  -- ^ the JWT/token expiry time, if any
  , wscsReqHeaders   :: ![H.Header]
  -- ^ headers from the client (in conn params) to forward to the remote schema
  , wscsIpAddress    :: !Wai.IpAddress
  -- ^ IP address required for 'MonadGQLAuthorization'
  }
  deriving (Show)

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser      :: !(STM.TVar WSConnState)
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap     :: !OperationMap
  , _wscErrRespTy :: !ErrRespType
  , _wscAPIType   :: !E.GraphQLQueryType
  }

data WSServerEnv
  = WSServerEnv
  { _wseLogger          :: !(L.Logger L.Hasura)
  , _wseLiveQMap        :: !LQ.LiveQueriesState
  , _wseGCtxMap         :: !(IO (SchemaCache, SchemaCacheVer))
  -- ^ an action that always returns the latest version of the schema cache. See 'SchemaCacheRef'.
  , _wseHManager        :: !H.Manager
  , _wseCorsPolicy      :: !CorsPolicy
  , _wseSQLCtx          :: !SQLGenCtx
  , _wseServer          :: !WSServer
  , _wseEnableAllowlist :: !Bool
  , _wseKeepAliveDelay  :: !KeepAliveDelay
  , _wseServerMetrics   :: !ServerMetrics
  }

type OperationMap = STMMap.Map OperationId (LQ.LiveQueryId, Maybe OperationName)

type WSServer = WS.WSServer WSConnData

type WSConn = WS.WSConn WSConnData
