module Hasura.GraphQL.Transport.WebSocket.Transaction.Handlers
  ( onMessageHandler
  , onCloseHandler
  , onConnHandler
  ) where

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Common
import           Hasura.GraphQL.Transport.WebSocket.Transaction.Protocol
import           Hasura.GraphQL.Transport.WebSocket.Transaction.Types
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Auth                                      (AuthMode,
                                                                          getUserInfoWithExpTime)
import           Hasura.Server.Init                                      (readIsoLevel)
import           Hasura.Server.Utils

import           Control.Concurrent                                      (threadDelay)
import           Data.Aeson

import qualified Hasura.GraphQL.Execute                                  as E
import qualified Hasura.GraphQL.Transport.WebSocket.Server               as WS
import qualified Hasura.Logging                                          as L

import qualified Control.Concurrent.STM                                  as STM
import qualified Data.ByteString.Lazy                                    as BL
import qualified Data.IORef                                              as IORef
import qualified Data.Text                                               as T
import qualified Data.Time.Clock                                         as TC
import qualified Database.PG.Query                                       as PG
import qualified Network.HTTP.Client                                     as H
import qualified Network.HTTP.Types                                      as H
import qualified Network.WebSockets                                      as WS

onConnHandler
  :: L.Logger
  -> AuthMode
  -> H.Manager
  -> PGExecCtx
  -> WS.OnConnH ConnState
onConnHandler lg@(L.Logger logger) authMode httpMgr pgExecCtx wsId requestHead = do
  resE <- runExceptT resolveAll
  case resE of
    Left e -> reject e
    Right (pgConn, localPool, userInfo, expTyM, errTy) -> do
      txStatus <- liftIO $ STM.newTVarIO TxBegin
      let acceptRequest = WS.defaultAcceptRequest
                      { WS.acceptSubprotocol = Just "graphql-tx"}
          pgConnCtx = PGConnCtx pgConn localPool
          connData = WSTxData userInfo errTy pgConnCtx txStatus
      logger $ mkInfoLog wsId EAccepted
      pure $ Right $ WS.AcceptWith (CSTransaction connData) acceptRequest Nothing (jwtExpiryHandler <$> expTyM)
  where
    PGExecCtx pool txIso = pgExecCtx
    resolveIsolevelHeader headers = do
      let isoLevelHeader = "x-hasura-tx-isolation"
      case getRequestHeader isoLevelHeader headers of
        Nothing  -> pure txIso
        Just val -> either (throw404 . ((bsToTxt isoLevelHeader <> ": ") <>) . T.pack)
                    pure $ readIsoLevel $ T.unpack $ bsToTxt val

    jwtExpiryHandler expTime _ = do
      currTime <- TC.getCurrentTime
      threadDelay $ diffTimeToMicro $ TC.diffUTCTime expTime currTime

    resolveAll = do
      errTy <- WS.checkPath requestHead
      let headers = WS.requestHeaders requestHead
      (userInfo, expTyM) <- getUserInfoWithExpTime lg httpMgr headers authMode
      maybePGConn <- liftIO $ PG.getPGConnMaybe pool
      (pgConn, localPool) <- maybe (throw404 "unable to acquire connection from pool, please try again") pure maybePGConn
      -- Run begin transaction
      txIsoLevel <- resolveIsolevelHeader headers
      runLazyTxWithConn pgConn $ beginTx txIsoLevel
      pure (pgConn, localPool, userInfo, expTyM, errTy)

    reject qErr = do
      logger $ mkErrorLog wsId $ ERejected qErr
      pure $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ encode $ encodeGQLErr False qErr)

onMessageHandler :: WSServerEnv -> WSTxData -> WSConn -> BL.ByteString -> IO ()
onMessageHandler serverEnv wsTxData wsConn rawMessage =
  case eitherDecode rawMessage of
    Left e -> do
      let errMsg = ErrorMessage Nothing wsId $ errFn $ err400 BadRequest $
                   "parsing ClientMessage failed: " <> T.pack e
      sendMsg wsConn $ SMError errMsg
    Right msg -> case msg of
      CMExecute (ExecutePayload maybeReqId query) -> do
        reqId <- liftIO $ maybe (RequestId <$> generateFingerprint) pure maybeReqId
        handleError (Just reqId) $ execute reqId query
      CMAbort           ->
        handleError Nothing $ do
          logOp OAbort
          runLazyTxWithConn pgConn abortTx
          modifyTxStatus TxAbort
          pure $ SMClose wsId "Executed 'ABORT' command"
      CMCommit          ->
        handleError Nothing $ do
          logOp OCommit
          runLazyTxWithConn pgConn commitTx
          modifyTxStatus TxCommit
          pure $ SMClose wsId "Executed 'COMMIT' command"
  where
    WSServerEnv lg@(L.Logger logger) pgExecCtx _ gCtxMapRef hMgr _ sqlGenCtx planCache _ enableAL = serverEnv
    wsId = WS.getWSId wsConn
    pgConn = _pccConn $ _wtdPgConn wsTxData
    userInfo = _wtdUserInfo wsTxData
    errTy = _wtdErrorType wsTxData
    logOp op = liftIO $ logger $ mkInfoLog wsId $ EOperation op
    modifyTxStatus status = liftIO $ STM.atomically $ STM.writeTVar (_wtdTxStatus wsTxData) status

    execute :: RequestId -> GQLReqUnparsed -> ExceptT QErr IO ServerMessage
    execute reqId query = do
      (sc, scVer) <- liftIO $ IORef.readIORef gCtxMapRef
      execPlan <- E.getResolvedExecPlan pgExecCtx
                   planCache userInfo sqlGenCtx enableAL sc scVer hMgr query
      case execPlan of
        E.GExPHasura resolvedOp -> do
          logOp $ OExecute $ ExecuteQuery reqId query
          (tx, genSql) <- case resolvedOp of
            E.ExOpQuery queryTx genSql -> pure (queryTx, genSql)
            E.ExOpMutation mutationTx -> pure (mutationTx, Nothing)
            E.ExOpSubs _ -> throw400 NotSupported "Subscriptions are not allowed in graphql transactions"
          lift $ logger $ QueryLog query genSql reqId
          res <- runLazyTxWithConn pgConn tx
          pure $ SMData $ DataMessage reqId wsId $ GRHasura $ GQSuccess $ encJToLBS res
        E.GExPRemote _ _  ->
          throw400 NotSupported "Remote server queries are not supported over graphql transactions"

    handleError :: Maybe RequestId -> ExceptT QErr IO ServerMessage -> IO ()
    handleError maybeReqId action = do
      resE <- runExceptT action
      case resE of
        Right m -> do
          sendMsg wsConn m
          case m of
            SMClose _ _ -> liftIO $ do
              WS.closeConn wsConn "Closing connection after 'Commit' and 'Abort'"
            _ -> pure ()
        Left e -> do
          logger $ mkErrorLog wsId $ EQueryError e
          sendMsg wsConn $ SMError $ ErrorMessage maybeReqId wsId $ errFn e
          when (qeError e == "connection error") $ do
            liftIO $ WS.closeConn wsConn "PG Connection error occured, closing the connection now"

    errFn =
      let isAdmin' = isAdmin $ userRole userInfo
      in case errTy of
           WS.ERTLegacy           -> encodeQErr isAdmin'
           WS.ERTGraphqlCompliant -> encodeGQLErr isAdmin'

onCloseHandler
  :: L.Logger
  -> WS.WSId
  -> WSTxData
  -> IO ()
onCloseHandler (L.Logger logger) wsId wsTxData = do
  txStatus <- liftIO $ STM.atomically $ STM.readTVar txStatusTVar
  when (txStatus == TxBegin) $ do
    -- If transaction is not committed or aborted, abort now
    eRes <- runExceptT $ runLazyTxWithConn pgConn abortTx
    either (logger . mkErrorLog wsId . EQueryError) pure eRes
  logger $ mkInfoLog wsId EClosed
  liftIO $ PG.returnPGConnToPool localPool pgConn
  where
    txStatusTVar = _wtdTxStatus wsTxData
    PGConnCtx pgConn localPool = _wtdPgConn wsTxData

mkInfoLog :: WS.WSId -> WSEvent -> WSLog
mkInfoLog wsId event = WSLog L.LevelInfo $ WSLogInfo wsId event

mkErrorLog :: WS.WSId -> WSEvent -> WSLog
mkErrorLog wsId event = WSLog L.LevelError $ WSLogInfo wsId event

sendMsg :: (MonadIO m) => WSConn -> ServerMessage -> m ()
sendMsg wsConn =
  liftIO . WS.sendMsg wsConn . encodeServerMessage
