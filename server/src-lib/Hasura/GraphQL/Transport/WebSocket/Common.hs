module Hasura.GraphQL.Transport.WebSocket.Common where

import           Hasura.Prelude

import qualified Control.Concurrent.STM                               as STM
import qualified Data.Time.Clock                                      as TC
import qualified Network.HTTP.Client                                  as H
import qualified Network.HTTP.Types                                   as H

import           Control.Concurrent.Extended                          (sleep)


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
  , _wseGCtxMap         :: !(IO (SchemaCache, SchemaCacheVer))
  -- ^ an action that always returns the latest version of the schema cache
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
  -> IO (SchemaCache, SchemaCacheVer)
  -> H.Manager
  -> CorsPolicy
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> m WSServerEnv
createWSServerEnv logger pgExecCtx lqState getSchemaCache httpManager
  corsPolicy sqlGenCtx enableAL planCache = do
  wsServer <- liftIO $ STM.atomically $ WS.createWSServer logger
  return $
    WSServerEnv logger pgExecCtx lqState getSchemaCache httpManager corsPolicy
    sqlGenCtx planCache wsServer enableAL

filterWsHeaders :: [H.Header] -> [H.Header]
filterWsHeaders hdrs = flip filter hdrs $ \(n, _) ->
  n `notElem` [ "sec-websocket-key"
              , "sec-websocket-version"
              , "sec-websocket-protocol"
              , "upgrade"
              , "connection"
              ]

tokenExpiryHandler :: WSConn -> IO ()
tokenExpiryHandler wsConn = do
  let connData = WS.getData wsConn
  expiryTime <- liftIO $ STM.atomically $ do
    maybeExpiryTime <- case connData of
      CSQueries queryData -> do
        connState <- STM.readTVar $ WQT._wscUser queryData
        case connState of
          WQT.CSNotInitialised _         -> STM.retry
          WQT.CSInitError _              -> STM.retry
          WQT.CSInitialised _ expTimeM _ -> pure expTimeM
      CSTransaction txData -> do
        txStatus <- STM.readTVar $ WTT._wtdTxStatus txData
        case txStatus of
          WTT.TxNotInitialised _   -> STM.retry
          WTT.TxBegin _ expTimeM _ -> pure expTimeM
          WTT.TxCommit             -> STM.retry
          WTT.TxAbort              -> STM.retry
    maybe STM.retry pure maybeExpiryTime

  currTime <- TC.getCurrentTime
  sleep $ convertDuration $ TC.diffUTCTime expiryTime currTime
