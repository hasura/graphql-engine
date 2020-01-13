module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  , stopWSServerApp
  , WSServerEnv
  ) where

import qualified Control.Concurrent.Async.Lifted.Safe                    as LA
import qualified Control.Monad.Trans.Control                             as MC
import qualified Data.Aeson                                              as J
import qualified Data.ByteString.Lazy                                    as BL
import qualified Data.Text                                               as T
import qualified Network.HTTP.Types                                      as H
import qualified Network.WebSockets                                      as WS


import           Hasura.GraphQL.Transport.WebSocket.Common
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth                                      (AuthMode,
                                                                          UserAuthentication)

import qualified Hasura.GraphQL.Execute.LiveQuery                        as LQ
import qualified Hasura.GraphQL.Transport.WebSocket.Queries.Handlers     as WQH
import qualified Hasura.GraphQL.Transport.WebSocket.Server               as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Transaction.Handlers as WTH
import qualified Hasura.Logging                                          as L

onConn
  :: (MonadIO m)
  => WSServerEnv -> WS.OnConnH m ConnState
onConn serverEnv wsId requestHead = do
  let requestHeaders = WS.requestHeaders requestHead
  case find (\h -> fst h == "sec-websocket-protocol") requestHeaders of
    Nothing      -> queryConnHandler
    Just (_, bs) -> do
      let valueText = bsToTxt bs
          headerVals = map T.strip $ T.splitOn "," valueText
      case headerVals of
        []               -> queryConnHandler
        ("graphql-ws":_) -> queryConnHandler
        ("graphql-tx":_) -> txConnHandler
        _                -> reject $ err404 BadRequest $ "protocol not supported: " <> valueText
    where
      logger = _wseLogger serverEnv
      corsPolicy = _wseCorsPolicy serverEnv
      pgExecCtx = _wseRunTx serverEnv
      queryConnHandler = WQH.onConnHandler logger corsPolicy wsId requestHead
      txConnHandler = WTH.onConnHandler logger pgExecCtx wsId requestHead
      reject qErr = pure $ Left $ WS.RejectRequest
                    (H.statusCode $ qeStatus qErr)
                    (H.statusMessage $ qeStatus qErr) []
                    (BL.toStrict $ J.encode $ encodeGQLErr False qErr)

onMessage
  :: (MonadIO m, UserAuthentication m)
  => AuthMode -> WSServerEnv -> WS.OnMessageH m ConnState
onMessage authMode serverEnv wsConn bytes =
  case WS.getData wsConn of
    CSQueries connData      -> WQH.onMessageHandler authMode serverEnv connData wsConn bytes
    CSTransaction pgConnCtx -> WTH.onMessageHandler authMode serverEnv pgConnCtx wsConn bytes

onClose :: MonadIO m => L.Logger L.Hasura -> LQ.LiveQueriesState -> WS.OnCloseH m ConnState
onClose logger lqMap wsConn =
  case WS.getData wsConn of
    CSQueries connData     -> WQH.onCloseHandler logger lqMap connData wsConn
    CSTransaction wsTxData -> WTH.onCloseHandler logger (WS.getWSId wsConn) wsTxData

createWSServerApp
  :: ( MonadIO m
     , MC.MonadBaseControl IO m
     , LA.Forall (LA.Pure m)
     , UserAuthentication m
     )
  => AuthMode
  -> WSServerEnv
  -> WS.PendingConnection -> m ()
  -- ^ aka generalized 'WS.ServerApp'
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      (onConn serverEnv)
      (onMessage authMode serverEnv)
      (onClose (_wseLogger serverEnv) $ _wseLiveQMap serverEnv)

stopWSServerApp :: WSServerEnv -> IO ()
stopWSServerApp wsEnv = WS.shutdown (_wseServer wsEnv)
