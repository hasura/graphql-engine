module Hasura.GraphQL.Transport.WebSocket.Common where

import           Hasura.Prelude

import qualified Control.Concurrent.STM                               as STM
import qualified Data.IORef                                           as IORef
import qualified Network.HTTP.Client                                  as H
import qualified Network.HTTP.Types                                   as H


import           Hasura.RQL.Types
import           Hasura.Server.Cors

import qualified Hasura.GraphQL.Execute                               as E
import qualified Hasura.GraphQL.Execute.LiveQuery                     as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Queries.Types     as WQT
import qualified Hasura.GraphQL.Transport.WebSocket.Server            as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Transaction.Types as WTT
import qualified Hasura.Logging                                       as L

data ConnState
  = CSQueries !WQT.WSConnData
  | CSTransaction !WTT.WSTxData

type WSConn = WS.WSConn ConnState
type WSServer = WS.WSServer ConnState

data WSServerEnv
  = WSServerEnv
  { _wseLogger          :: !(L.Logger L.Hasura)
  , _wseRunTx           :: !PGExecCtx
  , _wseLiveQMap        :: !LQ.LiveQueriesState
  , _wseGCtxMap         :: !(IORef.IORef (SchemaCache, SchemaCacheVer))
  , _wseHManager        :: !H.Manager
  , _wseCorsPolicy      :: !CorsPolicy
  , _wseSQLCtx          :: !SQLGenCtx
  , _wseQueryCache      :: !E.PlanCache
  , _wseServer          :: !WSServer
  , _wseEnableAllowlist :: !Bool
  }

createWSServerEnv
  :: (MonadIO m)
  => L.Logger L.Hasura
  -> PGExecCtx
  -> LQ.LiveQueriesState
  -> IORef.IORef (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> m WSServerEnv
createWSServerEnv logger pgExecCtx lqState cacheRef httpManager
  corsPolicy sqlGenCtx enableAL planCache = do
  wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
  return $
    WSServerEnv logger pgExecCtx lqState cacheRef httpManager corsPolicy
    sqlGenCtx planCache wsServer enableAL

filterWsHeaders :: [H.Header] -> [H.Header]
filterWsHeaders hdrs = flip filter hdrs $ \(n, _) ->
  n `notElem` [ "sec-websocket-key"
              , "sec-websocket-version"
              , "sec-websocket-protocol"
              , "upgrade"
              , "connection"
              ]
