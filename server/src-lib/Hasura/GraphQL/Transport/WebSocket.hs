{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  ) where

import qualified Control.Concurrent.Async                    as A
import qualified Control.Concurrent.STM                      as STM
import qualified Data.Aeson                                  as J
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.HashMap.Strict                         as Map
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified ListT
import qualified Network.HTTP.Types.Status                   as H
import qualified Network.WebSockets                          as WS
import qualified STMContainers.Map                           as STMMap

import           Control.Concurrent                          (threadDelay)
import           Data.IORef                                  (IORef, newIORef,
                                                              readIORef,
                                                              writeIORef)

import           Hasura.GraphQL.Resolve                      (resolveSelSet)
import           Hasura.GraphQL.Resolve.Context              (RespTx)
import qualified Hasura.GraphQL.Resolve.LiveQuery            as LQ
import           Hasura.GraphQL.Schema                       (GCtxMap, getGCtx)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import           Hasura.GraphQL.Validate                     (validateGQ)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth                          (AuthMode,
                                                              getUserInfo)

-- uniquely identifies an operation
type GOperationId = (WS.WSId, OperationId)

type TxRunner = RespTx -> IO (Either QErr BL.ByteString)

type OperationMap
  = STMMap.Map OperationId LQ.LiveQuery

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser  :: !(IORef (Maybe UserInfo))
  -- we only care about subscriptions,
  -- the other operations (query/mutations)
  -- are not tracked here
  , _wscOpMap :: !OperationMap
  }

type LiveQueryMap = LQ.LiveQueryMap GOperationId
type WSServer = WS.WSServer WSConnData

type WSConn = WS.WSConn WSConnData
sendMsg :: (MonadIO m) => WSConn -> ServerMsg -> m ()
sendMsg wsConn =
  liftIO . WS.sendMsg wsConn . encodeServerMsg
  -- liftIO . WS.sendMsg wsConn . traceShowId . encodeServerMsg

data WSServerEnv
  = WSServerEnv
  { _wseServer   :: !WSServer
  , _wseRunTx    :: !TxRunner
  , _wseLiveQMap :: !LiveQueryMap
  , _wseGCtxMap  :: !(IORef (SchemaCache, GCtxMap))
  }

onConn :: WS.OnConnH WSConnData
onConn wsId requestHead = do
  res <- runExceptT checkPath
  either (return . Left . toRejectReq) (fmap Right . onSuccess) res
  where
    keepAliveAction wsConn = forever $ do
      sendMsg wsConn SMConnKeepAlive
      threadDelay $ 5 * 1000 * 1000

    onSuccess _ = do
      connData <- WSConnData <$> newIORef Nothing <*> STMMap.newIO
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return (connData, acceptRequest, Just keepAliveAction)

    toRejectReq qErr =
      WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeQErr False qErr)

    checkPath =
      when (WS.requestPath requestHead /= "/v1alpha1/graphql") $
      throw404 "only /v1alpha1/graphql is supported on websockets"

onStart :: WSServerEnv -> WSConn -> StartMsg -> IO ()
onStart serverEnv wsConn (StartMsg opId q) = catchAndSend $ do

  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withExceptT preExecErr $
    throw400 UnexpectedPayload $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ readIORef userInfoR
  userInfo <- case userInfoM of
    Just userInfo -> return userInfo
    Nothing       -> throwError $
      SMConnErr "start received before the connection is initialised"

  -- validate and build tx
  gCtxMap <- fmap snd $ liftIO $ readIORef gCtxMapRef
  let gCtx = getGCtx (userRole userInfo) gCtxMap
  (opTy, fields) <- withExceptT preExecErr $ runReaderT (validateGQ q) gCtx
  let qTx = resolveSelSet userInfo gCtx opTy fields

  case opTy of
    G.OperationTypeSubscription -> do
      let lq = LQ.LiveQuery userInfo q
      liftIO $ STM.atomically $ STMMap.insert lq opId opMap
      liftIO $ LQ.addLiveQuery runTx lqMap lq
       qTx (wsId, opId) liveQOnChange

    _ -> withExceptT postExecErr $ do
      resp <- ExceptT $ runTx qTx
      sendMsg wsConn $ SMData $ DataMsg opId $ GQSuccess resp
      sendMsg wsConn $ SMComplete $ CompletionMsg opId

  where
    (WSServerEnv _ runTx lqMap gCtxMapRef) = serverEnv
    wsId = WS.getWSId wsConn
    (WSConnData userInfoR opMap) = WS.getData wsConn

    -- on change, send message on the websocket
    liveQOnChange resp = WS.sendMsg wsConn $ encodeServerMsg $ SMData $
                        DataMsg opId resp

    preExecErr qErr  = SMErr $ ErrorMsg opId $ encodeQErr False qErr
    postExecErr qErr = SMData $ DataMsg opId $ GQExecError
                       [encodeQErr False qErr]

    catchAndSend :: ExceptT ServerMsg IO () -> IO ()
    catchAndSend m = do
      res <- runExceptT m
      either (sendMsg wsConn) return res

onMessage
  :: AuthMode
  -> WSServerEnv
  -> WSConn -> BL.ByteString -> IO ()
onMessage authMode serverEnv wsConn msgRaw =
  case J.eitherDecode msgRaw of
    Left e    -> sendMsg wsConn $ SMConnErr $
                 ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
    Right msg -> case msg of
      CMConnInit params -> onConnInit wsConn authMode params
      CMStart startMsg  -> onStart serverEnv wsConn startMsg
      CMStop stopMsg    -> onStop serverEnv wsConn stopMsg
      CMConnTerm        -> WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"

onStop :: WSServerEnv -> WSConn -> StopMsg -> IO ()
onStop serverEnv wsConn (StopMsg opId) = do
  -- probably wrap the whole thing in a single tx?
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just liveQ -> LQ.removeLiveQuery lqMap liveQ (wsId, opId)
    Nothing    -> return ()
  STM.atomically $ STMMap.delete opId opMap
  where
    lqMap = _wseLiveQMap serverEnv
    wsId  = WS.getWSId wsConn
    opMap = _wscOpMap $ WS.getData wsConn

onConnInit :: (MonadIO m) => WSConn -> AuthMode -> ConnParams -> m ()
onConnInit wsConn authMode connParams = do
  res <- runExceptT $ getUserInfo headers authMode
  case res of
    Left e  ->
      liftIO $ WS.closeConn wsConn $
      BL.fromStrict $ TE.encodeUtf8 $ qeError e
    Right userInfo -> do
      liftIO $ writeIORef (_wscUser $ WS.getData wsConn) $ Just userInfo
      sendMsg wsConn SMConnAck
      -- TODO: send it periodically? Why doesn't apollo's protocol use
      -- ping/pong frames of websocket spec?
      sendMsg wsConn SMConnKeepAlive
  where
    headers = [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
              | (h, v) <- maybe [] Map.toList $ _cpHeaders connParams
              ]

onClose
  :: LiveQueryMap
  -> WS.ConnectionException
  -> WSConn
  -> IO ()
onClose lqMap _ wsConn = do
  operations <- STM.atomically $ ListT.toList $ STMMap.stream opMap
  void $ A.forConcurrently operations $ \(opId, liveQ) ->
    LQ.removeLiveQuery lqMap liveQ (wsId, opId)
  where
    wsId  = WS.getWSId wsConn
    opMap = _wscOpMap $ WS.getData wsConn

createWSServerEnv :: IORef (SchemaCache, GCtxMap) -> TxRunner -> IO WSServerEnv
createWSServerEnv gCtxMapRef runTx = do
  (wsServer, lqMap) <-
    STM.atomically $ (,) <$> WS.createWSServer <*> LQ.newLiveQueryMap
  return $ WSServerEnv wsServer runTx lqMap gCtxMapRef

createWSServerApp :: AuthMode -> WSServerEnv -> WS.ServerApp
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      onConn
      (onMessage authMode serverEnv)
      (onClose $ _wseLiveQMap serverEnv)
