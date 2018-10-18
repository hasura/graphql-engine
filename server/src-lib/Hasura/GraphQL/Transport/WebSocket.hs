{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.GraphQL.Transport.WebSocket
  ( createWSServerApp
  , createWSServerEnv
  ) where

import qualified Control.Concurrent.Async                    as A
import qualified Control.Concurrent.STM                      as STM
import qualified Data.Aeson                                  as J
import qualified Data.Aeson.Casing                           as J
import qualified Data.Aeson.TH                               as J
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.CaseInsensitive                        as CI
import qualified Data.HashMap.Strict                         as Map
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import qualified Language.GraphQL.Draft.Syntax               as G
import qualified ListT
import qualified Network.HTTP.Client                         as H
import qualified Network.HTTP.Types.Status                   as H
import qualified Network.WebSockets                          as WS
import qualified STMContainers.Map                           as STMMap

import           Control.Concurrent                          (threadDelay)
import qualified Data.IORef                                  as IORef

import           Hasura.GraphQL.Resolve                      (resolveSelSet)
import           Hasura.GraphQL.Resolve.Context              (RespTx)
import qualified Hasura.GraphQL.Resolve.LiveQuery            as LQ
import           Hasura.GraphQL.Schema                       (GCtxMap, getGCtx)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Protocol
import qualified Hasura.GraphQL.Transport.WebSocket.Server   as WS
import           Hasura.GraphQL.Validate                     (validateGQ)
import qualified Hasura.Logging                              as L
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth                          (AuthMode,
                                                              getUserInfo)
import qualified Hasura.Server.Query                         as RQ

-- uniquely identifies an operation
type GOperationId = (WS.WSId, OperationId)

type TxRunner = RespTx -> IO (Either QErr BL.ByteString)

type OperationMap
  = STMMap.Map OperationId LQ.LiveQuery

data WSConnData
  = WSConnData
  -- the role and headers are set only on connection_init message
  { _wscUser  :: !(IORef.IORef (Maybe (Either Text UserInfo)))
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
  { _wslWebsocketId :: !WS.WSId
  , _wslEvent       :: !WSEvent
  } deriving (Show, Eq)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''WSLog)

instance L.ToEngineLog WSLog where
  toEngineLog wsLog =
    (L.LevelInfo, "ws-handler", J.toJSON wsLog)

data WSServerEnv
  = WSServerEnv
  { _wseLogger   :: !L.Logger
  , _wseServer   :: !WSServer
  , _wseRunTx    :: !TxRunner
  , _wseLiveQMap :: !LiveQueryMap
  , _wseGCtxMap  :: !(IORef.IORef (SchemaCache, GCtxMap))
  , _wseHManager :: !H.Manager
  }

onConn :: L.Logger -> WS.OnConnH WSConnData
onConn (L.Logger logger) wsId requestHead = do
  res <- runExceptT checkPath
  either reject accept res
  where

    keepAliveAction wsConn = forever $ do
      sendMsg wsConn SMConnKeepAlive
      threadDelay $ 5 * 1000 * 1000

    accept _ = do
      logger $ WSLog wsId EAccepted
      connData <- WSConnData <$> IORef.newIORef Nothing <*> STMMap.newIO
      let acceptRequest = WS.defaultAcceptRequest
                          { WS.acceptSubprotocol = Just "graphql-ws"}
      return $ Right (connData, acceptRequest, Just keepAliveAction)

    reject qErr = do
      logger $ WSLog wsId $ ERejected qErr
      return $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ J.encode $ encodeQErr False qErr)

    checkPath =
      when (WS.requestPath requestHead /= "/v1alpha1/graphql") $
      throw404 "only /v1alpha1/graphql is supported on websockets"

onStart :: WSServerEnv -> WSConn -> StartMsg -> IO ()
onStart serverEnv wsConn (StartMsg opId q) = catchAndIgnore $ do

  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap

  when (isJust opM) $ withComplete $ sendConnErr $
    "an operation already exists with this id: " <> unOperationId opId

  userInfoM <- liftIO $ IORef.readIORef userInfoR
  userInfo <- case userInfoM of
    Just (Right userInfo) -> return userInfo
    Just (Left initErr) -> do
      let connErr = "cannot start as connection_init failed with : " <> initErr
      withComplete $ sendConnErr connErr
    Nothing       -> do
      let connErr = "start received before the connection is initialised"
      withComplete $ sendConnErr connErr

  -- validate and build tx
  gCtxMap <- fmap snd $ liftIO $ IORef.readIORef gCtxMapRef
  let gCtx = getGCtx (userRole userInfo) gCtxMap

  (opTy, fields) <- either (withComplete . preExecErr) return $
                    runReaderT (validateGQ q) gCtx
  let qTx = RQ.setHeadersTx userInfo >>
            resolveSelSet userInfo gCtx opTy fields

  case opTy of
    G.OperationTypeSubscription -> do
      let lq = LQ.LiveQuery userInfo q
      liftIO $ STM.atomically $ STMMap.insert lq opId opMap
      liftIO $ LQ.addLiveQuery runTx lqMap lq
       qTx (wsId, opId) liveQOnChange
      logOpEv ODStarted
    _ ->  do
      logOpEv ODStarted
      resp <- liftIO $ runTx qTx
      either postExecErr sendSuccResp resp
      sendCompleted

  where
    WSServerEnv (L.Logger logger) _ runTx lqMap gCtxMapRef _ = serverEnv
    wsId = WS.getWSId wsConn
    WSConnData userInfoR opMap = WS.getData wsConn

    logOpEv opDet =
      liftIO $ logger $ WSLog wsId $
      EOperation opId (_grOperationName q) opDet

    sendConnErr connErr = do
      sendMsg wsConn $ SMErr $ ErrorMsg opId $ J.toJSON connErr
      logOpEv $ ODProtoErr connErr

    sendCompleted = do
      sendMsg wsConn $ SMComplete $ CompletionMsg opId
      logOpEv ODCompleted

    postExecErr qErr = do
      logOpEv $ ODQueryErr qErr
      sendMsg wsConn $ SMData $ DataMsg opId $
        GQExecError $ pure $ encodeQErr False qErr

    -- why wouldn't pre exec error use graphql response?
    preExecErr qErr = do
      logOpEv $ ODQueryErr qErr
      sendMsg wsConn $ SMErr $ ErrorMsg opId $ encodeQErr False qErr

    sendSuccResp bs =
      sendMsg wsConn $ SMData $ DataMsg opId $ GQSuccess bs

    withComplete :: ExceptT () IO () -> ExceptT () IO a
    withComplete action = do
      action
      sendCompleted
      throwError ()

    -- on change, send message on the websocket
    liveQOnChange resp =
      WS.sendMsg wsConn $ encodeServerMsg $ SMData $ DataMsg opId resp

    catchAndIgnore :: ExceptT () IO () -> IO ()
    catchAndIgnore m = void $ runExceptT m

onMessage
  :: AuthMode
  -> WSServerEnv
  -> WSConn -> BL.ByteString -> IO ()
onMessage authMode serverEnv wsConn msgRaw =
  case J.eitherDecode msgRaw of
    Left e    -> do
      let err = ConnErrMsg $ "parsing ClientMessage failed: " <> T.pack e
      liftIO $ logger $ WSLog (WS.getWSId wsConn) $ EConnErr err
      sendMsg wsConn $ SMConnErr err

    Right msg -> case msg of
      CMConnInit params -> onConnInit (_wseLogger serverEnv)
                           (_wseHManager serverEnv)
                           wsConn authMode params
      CMStart startMsg  -> onStart serverEnv wsConn startMsg
      CMStop stopMsg    -> onStop serverEnv wsConn stopMsg
      CMConnTerm        -> WS.closeConn wsConn "GQL_CONNECTION_TERMINATE received"
  where
    logger = L.unLogger $ _wseLogger serverEnv

onStop :: WSServerEnv -> WSConn -> StopMsg -> IO ()
onStop serverEnv wsConn (StopMsg opId) = do
  -- probably wrap the whole thing in a single tx?
  opM <- liftIO $ STM.atomically $ STMMap.lookup opId opMap
  case opM of
    Just liveQ -> do
      let opNameM = _grOperationName $ LQ._lqRequest liveQ
      liftIO $ logger $ WSLog wsId $ EOperation opId opNameM ODStopped
      LQ.removeLiveQuery lqMap liveQ (wsId, opId)
    Nothing    -> return ()
  STM.atomically $ STMMap.delete opId opMap
  where
    logger = L.unLogger $ _wseLogger serverEnv
    lqMap  = _wseLiveQMap serverEnv
    wsId   = WS.getWSId wsConn
    opMap  = _wscOpMap $ WS.getData wsConn

onConnInit
  :: (MonadIO m)
  => L.Logger -> H.Manager -> WSConn -> AuthMode -> Maybe ConnParams -> m ()
onConnInit (L.Logger logger) manager wsConn authMode connParamsM = do
  res <- runExceptT $ getUserInfo logger manager headers authMode
  case res of
    Left e  -> do
      liftIO $ IORef.writeIORef (_wscUser $ WS.getData wsConn) $
        Just $ Left $ qeError e
      let connErr = ConnErrMsg $ qeError e
      liftIO $ logger $ WSLog (WS.getWSId wsConn) $ EConnErr connErr
      sendMsg wsConn $ SMConnErr connErr
    Right userInfo -> do
      liftIO $ IORef.writeIORef (_wscUser $ WS.getData wsConn) $
        Just $ Right userInfo
      sendMsg wsConn SMConnAck
      -- TODO: send it periodically? Why doesn't apollo's protocol use
      -- ping/pong frames of websocket spec?
      sendMsg wsConn SMConnKeepAlive
  where
    headers = [ (CI.mk $ TE.encodeUtf8 h, TE.encodeUtf8 v)
              | (h, v) <- maybe [] Map.toList $ connParamsM >>= _cpHeaders
              ]

onClose
  :: L.Logger
  -> LiveQueryMap
  -> WS.ConnectionException
  -> WSConn
  -> IO ()
onClose (L.Logger logger) lqMap _ wsConn = do
  logger $ WSLog wsId EClosed
  operations <- STM.atomically $ ListT.toList $ STMMap.stream opMap
  void $ A.forConcurrently operations $ \(opId, liveQ) ->
    LQ.removeLiveQuery lqMap liveQ (wsId, opId)
  where
    wsId  = WS.getWSId wsConn
    opMap = _wscOpMap $ WS.getData wsConn

createWSServerEnv
  :: L.Logger
  -> H.Manager -> IORef.IORef (SchemaCache, GCtxMap)
  -> TxRunner -> IO WSServerEnv
createWSServerEnv logger httpManager gCtxMapRef runTx = do
  (wsServer, lqMap) <-
    STM.atomically $ (,) <$> WS.createWSServer logger <*> LQ.newLiveQueryMap
  return $ WSServerEnv logger wsServer runTx lqMap gCtxMapRef httpManager

createWSServerApp :: AuthMode -> WSServerEnv -> WS.ServerApp
createWSServerApp authMode serverEnv =
  WS.createServerApp (_wseServer serverEnv) handlers
  where
    handlers =
      WS.WSHandlers
      (onConn $ _wseLogger serverEnv)
      (onMessage authMode serverEnv)
      (onClose (_wseLogger serverEnv) $ _wseLiveQMap serverEnv)
