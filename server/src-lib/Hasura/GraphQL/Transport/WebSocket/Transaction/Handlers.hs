{-# LANGUAGE RecordWildCards #-}
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
                                                                          UserAuthentication,
                                                                          resolveUserInfo)
import           Hasura.Server.Cors
import           Hasura.Server.Utils
import           Hasura.Server.Version
import           Hasura.Session

import           Data.Aeson

import qualified Hasura.GraphQL.Execute                                  as E
import qualified Hasura.GraphQL.Resolve.Action                           as RA
import qualified Hasura.GraphQL.Transport.WebSocket.Server               as WS
import qualified Hasura.Logging                                          as L
import qualified Hasura.Server.Telemetry.Counters                        as Telem

import qualified Control.Concurrent.STM                                  as STM
import qualified Data.ByteString.Lazy                                    as BL
import qualified Data.CaseInsensitive                                    as CI
import qualified Data.HashMap.Strict                                     as Map
import qualified Data.Text                                               as T
import qualified Database.PG.Query                                       as PG
import qualified Network.HTTP.Types                                      as H
import qualified Network.WebSockets                                      as WS

onConnHandler
  :: (MonadIO m)
  => L.Logger L.Hasura
  -> PGExecCtx
  -> CorsPolicy
  -> WS.OnConnH m ConnState
onConnHandler (L.Logger logger) pgExecCtx corsPolicy wsId requestHead = do
  resE <- runExceptT resolveAll
  case resE of
    Left e -> reject e
    Right (pgConn, localPool, errTy, headers) -> do
      txStatus <- liftIO $ STM.newTVarIO $ TxNotInitialised headers
      let acceptRequest = WS.defaultAcceptRequest
                      { WS.acceptSubprotocol = Just "graphql-tx"}
          pgConnCtx = PGConnCtx pgConn localPool
          connData = WSTxData errTy pgConnCtx txStatus
      logger $ mkInfoLog wsId EAccepted
      pure $ Right $ WS.AcceptWith (CSTransaction connData) acceptRequest Nothing tokenExpiryHandler
  where
    PGExecCtx pool _ = pgExecCtx
    resolveAll = do
      let logCorsNote corsNote = lift $ logger $ mkInfoLog wsId $ ECorsNote corsNote
      headers <- WS.getHeadersWithEnforceCors logCorsNote requestHead corsPolicy
      errTy <- WS.checkPath requestHead
      maybePGConn <- liftIO $ PG.getPGConnMaybe pool
      (pgConn, localPool) <- maybe
                             (throw404 "unable to acquire connection from pool, please try again")
                             pure maybePGConn
      pure (pgConn, localPool, errTy, headers)

    reject qErr = do
      logger $ mkErrorLog wsId $ ERejected qErr
      pure $ Left $ WS.RejectRequest
        (H.statusCode $ qeStatus qErr)
        (H.statusMessage $ qeStatus qErr) []
        (BL.toStrict $ encode $ encodeGQLErr False qErr)

onMessageHandler :: forall m. (HasVersion, MonadIO m, UserAuthentication m)
                 => AuthMode -> WSServerEnv -> WSTxData -> WSConn -> BL.ByteString -> m ()
onMessageHandler authMode serverEnv wsTxData wsConn rawMessage = do
  timerTot <- startTimer
  case eitherDecode rawMessage of
    Left e -> do
      let errMsg = ErrorMessage Nothing wsId $ encodeQErr True $ err400 BadRequest $
                   "parsing ClientMessage failed: " <> T.pack e
      sendMsg wsConn $ SMError errMsg
    Right msg -> case msg of
      CMInit initPayload       -> onInit initPayload
      CMExecute executePayload -> onExecute timerTot executePayload
      CMAbort                  -> runCommand do
        lift $ logOp OAbort
        runLazyTxWithConn pgConn abortTx
        modifyTxStatus TxAbort
        pure $ SMClose wsId "Executed 'ABORT' command"
      CMCommit                 -> runCommand do
        lift $ logOp OCommit
        runLazyTxWithConn pgConn commitTx
        modifyTxStatus TxCommit
        pure $ SMClose wsId "Executed 'COMMIT' command"
  where
    WSServerEnv lg@(L.Logger logger) pgExecCtx _ getSchemaCache manager _ sqlGenCtx planCache _ enableAL = serverEnv
    wsId = WS.getWSId wsConn
    pgConn = _pccConn $ _wtdPgConn wsTxData
    errTy = _wtdErrorType wsTxData
    txStatusTVar = _wtdTxStatus wsTxData
    logOp op = liftIO $ logEvent $ EOperation op
    logError ev = logger $ mkErrorLog wsId ev
    logEvent ev = logger $ mkInfoLog wsId ev
    modifyTxStatus status = liftIO $ STM.atomically $ STM.writeTVar txStatusTVar status

    withUser maybeReqId f = do
      userInfoE <- runExceptT getUserInfo
      case userInfoE of
        Left e -> do
          logError $ EQueryError e
          sendMsg wsConn $ SMError $ ErrorMessage maybeReqId wsId $ String $ qeError e
        Right userInfo -> f userInfo
      where
        getUserInfo = do
          txStatus <- liftIO $ STM.readTVarIO txStatusTVar
          case txStatus of
            TxNotInitialised _         -> throw500 "query received without transaction init"
            TxBegin userInfo _ headers -> pure (userInfo, headers)
            TxCommit                   -> throw500 "transaction already committed"
            TxAbort                    -> throw500 "transaction already aborted"

    onInit :: InitPayload -> m ()
    onInit (InitPayload (TxIsolation txIso) paramHeaders) = do
      txStatus <- liftIO $ STM.readTVarIO txStatusTVar

      eitherResult <- runExceptT $ case txStatus of
        TxNotInitialised clientHeaders -> do
          let reqHeaders = (Map.toList . Map.fromList) $
                           map ((CI.mk . txtToBs) *** txtToBs) (Map.toList paramHeaders)
                           <> clientHeaders
          userInfoE <- lift $ resolveUserInfo lg manager reqHeaders authMode
          (userInfo, expTime) <- liftEither userInfoE
          runLazyTxWithConn pgConn $ do
            -- Run BEGIN command
            beginTx txIso
            -- Set session variables
            liftTx $ setHeadersTx $ _uiSession userInfo
          modifyTxStatus $ TxBegin userInfo expTime reqHeaders

        _ -> throw500 "transaction cannot be initialised more than once in a single WebSocket session"

      case eitherResult of
        Left e -> do
          let errMsg = qeError e
          liftIO $ logEvent $ EInitErr errMsg
          sendMsg wsConn $ SMInitErr errMsg
        Right _ -> do
          liftIO $ logEvent EInitialised
          sendMsg wsConn SMInitialised

    onExecute :: m DiffTime -> ExecutePayload -> m ()
    onExecute timerTot (ExecutePayload maybeReqId query) = do
      reqId <- liftIO $ maybe (RequestId <$> generateFingerprint) pure maybeReqId
      withUser (Just reqId) $ \(userInfo, reqHeaders) -> do
        logOp $ OExecute $ ExecuteQuery reqId query userInfo
        (sc, scVer) <- liftIO getSchemaCache
        execPlanE <- runExceptT $ E.getResolvedExecPlan (RA.restrictActions "Actions are not allowed in graphql transactions")
                     pgExecCtx planCache userInfo sqlGenCtx enableAL sc scVer manager reqHeaders query
        case execPlanE of
          Left e -> do
            logError $ EQueryError e
            let err = case errTy of
                  WS.ERTLegacy           -> errFn userInfo e
                  -- Pre execute GraphQL error format
                  WS.ERTGraphqlCompliant -> object ["errors" .= [errFn userInfo e]]
            sendMsg wsConn $ SMError $ ErrorMessage (Just reqId) wsId err

          Right (telemCacheHit, execPlan) -> do
            eitherResult <- withElapsedTime $ runExceptT $ case execPlan of
              E.GExPHasura resolvedOp -> do
                (queryType, tx, genSql) <- case resolvedOp of
                  E.ExOpQuery queryTx genSql -> pure (Telem.Query, queryTx, genSql)
                  E.ExOpMutation _ mutationTx -> pure (Telem.Mutation, mutationTx, Nothing)
                  E.ExOpSubs _ -> throw400 NotSupported "Subscriptions are not allowed in graphql transactions"
                logger $ QueryLog query genSql reqId
                res <- runLazyTxWithConn pgConn tx
                pure $ (, queryType) $ SMData $ DataMessage reqId wsId $ GRHasura $ GQSuccess $ encJToLBS res
              E.GExPRemote _ _  ->
                throw400 NotSupported "Remote server queries are not supported over graphql transactions"

            case eitherResult of
              (_, Left e)  -> handleError userInfo (Just reqId) e
              (telemTimeIO_DT, Right (r, telemQueryType)) -> do
                -- Telemetry. NOTE: don't time network IO:
                telemTimeTot <- Seconds <$> timerTot
                sendMsg wsConn r
                let telemTimeIO = convertDuration telemTimeIO_DT
                    telemLocality = Telem.Local
                    telemTransport = Telem.WebSocket
                Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}


    runCommand :: ExceptT QErr m ServerMessage -> m ()
    runCommand action =
      withUser Nothing $ \(userInfo, _) -> do
        eitherResult <- runExceptT action
        case eitherResult of
          Left e -> handleError userInfo Nothing e
          Right sm -> do
            sendMsg wsConn sm
            liftIO $ WS.closeConn wsConn "Closing connection after 'commit' and 'abort'"


    handleError :: UserInfo -> Maybe RequestId -> QErr -> m ()
    handleError userInfo maybeReqId qErr = do
      logError $ EQueryError qErr
      sendMsg wsConn $ SMError $ ErrorMessage maybeReqId wsId $ errFn userInfo qErr
      when (qeError qErr == "connection error") $
        liftIO $ WS.closeConn wsConn "PG Connection error occured, closing the connection now"

    errFn userInfo =
      let isAdmin' = isAdmin $ _uiRole userInfo
      in case errTy of
           WS.ERTLegacy           -> encodeQErr isAdmin'
           WS.ERTGraphqlCompliant -> encodeGQLErr isAdmin'

onCloseHandler
  :: (MonadIO m)
  => L.Logger L.Hasura
  -> WS.WSId
  -> WSTxData
  -> m ()
onCloseHandler (L.Logger logger) wsId wsTxData = do
  txStatus <- liftIO $ STM.atomically $ STM.readTVar txStatusTVar
  case txStatus of
    TxBegin{} -> do
      -- If status is still 'Begin', abort now
      eRes <- runExceptT $ runLazyTxWithConn pgConn abortTx
      either (logger . mkErrorLog wsId . EQueryError) pure eRes
    _ -> pure ()
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
sendMsg wsConn msg =
  liftIO $ WS.sendMsg wsConn $ WS.WSQueueResponse (encodeServerMessage msg) Nothing
