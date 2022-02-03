{-# LANGUAGE UndecidableInstances #-}

-- | A module for running graphql-engine in a testing context.
module Hasura.Test.App
  ( TestM,
    runTestM,
    withHasuraTestApp,
  )
where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Managed (lowerManagedT)
import Data.Environment (mkEnvironment)
import Data.HashSet qualified as HS
import Data.Time.Clock (getCurrentTime)
import Database.PG.Query qualified as Q
import Hasura.App
  ( GlobalCtx (..),
    accessDeniedErrMsg,
    getCatalogStateTx,
    initGlobalCtx,
    initialiseServeCtx,
    mkHGEServer,
    mkMSSQLSourceResolver,
    mkPgSourceResolver,
    notifySchemaCacheSyncTx,
    setCatalogStateTx,
  )
import Hasura.Backends.Postgres.Connection (checkDbConnection)
import Hasura.Base.Error
  ( Code (AccessDenied),
    QErr,
    throw400,
    withPathK,
  )
import Hasura.Eventing.ScheduledTrigger
import Hasura.GraphQL.Execute (checkQueryInAllowlist)
import Hasura.GraphQL.Execute.Action
import Hasura.GraphQL.Execute.Backend (ExecutionStep (..), MonadQueryTags (..))
import Hasura.GraphQL.Execute.Common (MonadGQLExecutionCheck (..))
import Hasura.GraphQL.Logging (MonadQueryLog (..))
import Hasura.GraphQL.Transport.HTTP
  ( CacheStoreSuccess (CacheStoreSkipped),
    MonadExecuteQuery (..),
  )
import Hasura.GraphQL.Transport.HTTP.Protocol (toParsed)
import Hasura.GraphQL.Transport.WebSocket.Server (MonadWSLog (..))
import Hasura.Logging qualified as Logging
import Hasura.Metadata.Class
  ( MetadataStorageT (..),
    MonadMetadataStorage (..),
  )
import Hasura.Prelude
import Hasura.QueryTags (emptyQueryTagsComment)
import Hasura.RQL.DDL.Schema.Catalog
import Hasura.RQL.Types.Source (MonadResolveSource (..))
import Hasura.Server.API.Query (requiresAdmin)
import Hasura.Server.App
  ( ConsoleRenderer (..),
    HandlerCtx (hcUser),
    MonadConfigApiHandler (..),
    MonadMetadataApiAuthorization (..),
  )
import Hasura.Server.Auth
  ( AuthHookG (AuthHookG),
    UserAuthentication (..),
    getUserInfoWithExpTime,
  )
import Hasura.Server.Init (getDbId, logLevelEnv, mkServeOptions)
import Hasura.Server.Init.Config
  ( API (..),
    PostgresConnInfo (PostgresConnInfo),
    RawConnParams (RawConnParams),
    RawServeOptions (..),
    ServeOptions (soEnabledAPIs),
    runWithEnv,
  )
import Hasura.Server.Limits (HasResourceLimits (..), ResourceLimits (..))
import Hasura.Server.Logging (HttpLog (..))
import Hasura.Server.Metrics (ServerMetrics, createServerMetrics)
import Hasura.Session (UserInfo (_uiRole), adminRoleName)
import Hasura.Tracing (HasReporter (..), TraceT, noReporter)
import Network.Wai (Application)
import System.Environment (getEnvironment)
import System.Metrics qualified as EKG

--------------------------------------------------------------------------------

-- | A concrete monad for running the GraphQL-engine. This is the context in
-- which tests will run.
newtype TestM a = TestM {_runTestM :: TestConfig -> IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase IO,
      MonadBaseControl IO,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadReader TestConfig
    )
    via (ReaderT TestConfig IO)

-- | Exposed `TestM` deconstructor
runTestM :: TestM a -> TestConfig -> IO a
runTestM = _runTestM

-- | The testing environment
data TestConfig = TestConfig
  { tcPostgresPool :: Q.PGPool,
    tcPostgresLogger :: Q.PGLogger
  }

-- | Exposed `TestConfig` constructor
mkTestConfig :: Q.ConnInfo -> IO TestConfig
mkTestConfig pgConnInfo = do
  let pgLogger = print
      connParams = Q.defaultConnParams {Q.cpConns = 1}
  pgPool <- Q.initPGPool pgConnInfo connParams pgLogger
  pure $
    TestConfig
      { tcPostgresPool = pgPool,
        tcPostgresLogger = pgLogger
      }

--------------------------------------------------------------------------------
-- GraphQL-engine instances

-- Note: For consistency, the following typeclass instances should mirror those
-- used by the open source graphql-engine, unless we intend to mock certain
-- operations.

instance MonadMetadataStorage (MetadataStorageT TestM) where
  fetchMetadataResourceVersion = runInSeparateTx fetchMetadataResourceVersionFromCatalog
  fetchMetadata = runInSeparateTx fetchMetadataAndResourceVersionFromCatalog
  fetchMetadataNotifications a b = runInSeparateTx $ fetchMetadataNotificationsFromCatalog a b
  setMetadata r = runInSeparateTx . setMetadataInCatalog (Just r)
  notifySchemaCacheSync a b c = runInSeparateTx $ notifySchemaCacheSyncTx a b c
  getCatalogState = runInSeparateTx getCatalogStateTx
  setCatalogState a b = runInSeparateTx $ setCatalogStateTx a b

  getDatabaseUid = runInSeparateTx getDbId
  checkMetadataStorageHealth = lift (asks tcPostgresPool) >>= checkDbConnection

  getDeprivedCronTriggerStats = runInSeparateTx . getDeprivedCronTriggerStatsTx
  getScheduledEventsForDelivery = runInSeparateTx getScheduledEventsForDeliveryTx
  insertCronEvents = runInSeparateTx . insertCronEventsTx
  insertOneOffScheduledEvent = runInSeparateTx . insertOneOffScheduledEventTx
  insertScheduledEventInvocation a b = runInSeparateTx $ insertInvocationTx a b
  setScheduledEventOp a b c = runInSeparateTx $ setScheduledEventOpTx a b c
  unlockScheduledEvents a b = runInSeparateTx $ unlockScheduledEventsTx a b
  unlockAllLockedScheduledEvents = runInSeparateTx unlockAllLockedScheduledEventsTx
  clearFutureCronEvents = runInSeparateTx . dropFutureCronEventsTx
  getOneOffScheduledEvents a b = runInSeparateTx $ getOneOffScheduledEventsTx a b
  getCronEvents a b c = runInSeparateTx $ getCronEventsTx a b c
  getInvocations a b = runInSeparateTx $ getInvocationsTx a b
  deleteScheduledEvent a b = runInSeparateTx $ deleteScheduledEventTx a b

  insertAction a b c d = runInSeparateTx $ insertActionTx a b c d
  fetchUndeliveredActionEvents = runInSeparateTx fetchUndeliveredActionEventsTx
  setActionStatus a b = runInSeparateTx $ setActionStatusTx a b
  fetchActionResponse = runInSeparateTx . fetchActionResponseTx
  clearActionData = runInSeparateTx . clearActionDataTx
  setProcessingActionLogsToPending = runInSeparateTx . setProcessingActionLogsToPendingTx

runInSeparateTx :: Q.TxE QErr a -> MetadataStorageT TestM a
runInSeparateTx tx = do
  pool <- lift $ asks tcPostgresPool
  liftEitherM $ liftIO $ runExceptT $ Q.runTx pool (Q.RepeatableRead, Nothing) tx

instance MonadResolveSource TestM where
  getPGSourceResolver = mkPgSourceResolver <$> asks tcPostgresLogger
  getMSSQLSourceResolver = return mkMSSQLSourceResolver

instance UserAuthentication (TraceT TestM) where
  resolveUserInfo logger manager headers authMode reqs =
    runExceptT $ getUserInfoWithExpTime logger manager headers authMode reqs

instance MonadMetadataApiAuthorization TestM where
  authorizeV1QueryApi query handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (requiresAdmin query && currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

  authorizeV1MetadataApi _ handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

  authorizeV2QueryApi _ handlerCtx = runExceptT do
    let currRole = _uiRole $ hcUser handlerCtx
    when (currRole /= adminRoleName) $
      withPathK "args" $ throw400 AccessDenied accessDeniedErrMsg

instance MonadGQLExecutionCheck TestM where
  checkGQLExecution userInfo _ enableAL sc query = runExceptT $ do
    req <- toParsed query
    checkQueryInAllowlist enableAL userInfo req sc
    return req
  executeIntrospection _ introspectionQuery _ =
    pure $ Right $ ExecStepRaw introspectionQuery

instance HttpLog TestM where
  type ExtraHttpLogMetadata TestM = ()
  emptyExtraHttpLogMetadata = ()
  buildExtraHttpLogMetadata = const ()
  logHttpError _a _b _c _d _e _f _g _h = pure ()
  logHttpSuccess _a _b _c _d _e _f _g _h _i _j _k _l = pure ()

instance MonadQueryLog TestM where
  logQueryLog = Logging.unLogger

instance MonadQueryTags TestM where
  createQueryTags _qtSourceConfig _attributes = pure emptyQueryTagsComment

instance HasReporter TestM where
  askReporter = pure noReporter

instance HasResourceLimits TestM where
  askHTTPHandlerLimit = pure $ ResourceLimits id
  askGraphqlOperationLimit = pure $ \_ _ -> ResourceLimits id

instance MonadExecuteQuery TestM where
  cacheLookup _ _ _ _ = pure ([], Nothing)
  cacheStore _ _ _ = pure (Right CacheStoreSkipped)

-- Instances not currently needed

instance ConsoleRenderer TestM where
  renderConsole = error "renderConsole: unimplemented for testing"

instance MonadWSLog TestM where
  logWSLog = error "logWSLog: unimplemented for testing"

instance MonadConfigApiHandler TestM where
  runConfigApiHandler = error "runConfigApiHandler: unimplemented for testing"

--------------------------------------------------------------------------------

-- | Run an instance of the GraphQL-engine in a testing environment, exposed as
-- a WAI app.
withHasuraTestApp ::
  -- | Metadata database URL
  String ->
  (Application -> TestM a) ->
  IO a
withHasuraTestApp metadataDbUrl action = do
  let setupHook = \_ -> pure ()
      postPollHook = Nothing

  initTime <- getCurrentTime
  (ekgStore, serverMetrics) <- initMetrics

  rawEnv <- filter ((`elem` envAllowList) . fst) <$> getEnvironment
  let serveOptions = makeServeOptions rawEnv
      env = mkEnvironment rawEnv
  globalCtx <-
    initGlobalCtx env (Just metadataDbUrl) $ PostgresConnInfo Nothing Nothing
  testConfig <- mkTestConfig (_gcMetadataDbConnInfo globalCtx)

  flip runTestM testConfig $
    lowerManagedT $ do
      serveCtx <- initialiseServeCtx env globalCtx serveOptions
      waiApp <-
        mkHGEServer
          setupHook
          env
          serveOptions
          serveCtx
          initTime
          postPollHook
          serverMetrics
          ekgStore
      lift $ action waiApp

-- A whitelist of environment variables that are safe to use in testing
envAllowList :: [String]
envAllowList = [fst logLevelEnv]

-- For now, we use the default server options for simplicity.
makeServeOptions :: [(String, String)] -> ServeOptions Logging.Hasura
makeServeOptions env =
  case runWithEnv env (mkServeOptions defaultRawServeOptions) of
    Left errMsg -> error $ "makeServeOptions: " ++ errMsg
    Right opts -> setEnabledAPIs opts

-- Enabling only those APIs needed for testing
setEnabledAPIs ::
  ServeOptions Logging.Hasura -> ServeOptions Logging.Hasura
setEnabledAPIs serveOptions =
  serveOptions {soEnabledAPIs = HS.fromList [GRAPHQL, METADATA]}

initMetrics :: IO (EKG.Store EKG.EmptyMetrics, ServerMetrics)
initMetrics = do
  ekgStore <- liftIO EKG.newStore
  serverMetrics <- liftIO $ createServerMetrics ekgStore
  pure (EKG.subset EKG.emptyOf ekgStore, serverMetrics)

defaultRawServeOptions :: RawServeOptions impl
defaultRawServeOptions =
  RawServeOptions
    { rsoPort = Nothing,
      rsoHost = Nothing,
      rsoConnParams =
        RawConnParams
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing,
      rsoTxIso = Nothing,
      rsoAdminSecret = Nothing,
      rsoAuthHook = AuthHookG Nothing Nothing,
      rsoJwtSecret = Nothing,
      rsoUnAuthRole = Nothing,
      rsoCorsConfig = Nothing,
      rsoEnableConsole = False,
      rsoConsoleAssetsDir = Nothing,
      rsoEnableTelemetry = Nothing,
      rsoWsReadCookie = False,
      rsoStringifyNum = False,
      rsoDangerousBooleanCollapse = Nothing,
      rsoEnabledAPIs = Nothing,
      rsoMxRefetchInt = Nothing,
      rsoMxBatchSize = Nothing,
      rsoEnableAllowlist = False,
      rsoEnabledLogTypes = Nothing,
      -- Setting lowest log level by default in order to make the tests more
      -- legible
      rsoLogLevel = Just Logging.LevelError,
      rsoDevMode = False,
      rsoAdminInternalErrors = Nothing,
      rsoEventsHttpPoolSize = Nothing,
      rsoEventsFetchInterval = Nothing,
      rsoAsyncActionsFetchInterval = Nothing,
      rsoLogHeadersFromEnv = False,
      rsoEnableRemoteSchemaPermissions = False,
      rsoWebSocketCompression = False,
      rsoWebSocketKeepAlive = Nothing,
      rsoInferFunctionPermissions = Nothing,
      rsoEnableMaintenanceMode = False,
      rsoSchemaPollInterval = Nothing,
      rsoExperimentalFeatures = Nothing,
      rsoEventsFetchBatchSize = Nothing,
      rsoGracefulShutdownTimeout = Nothing,
      rsoWebSocketConnectionInitTimeout = Nothing,
      rsoOptimizePermissionFilters = Nothing
    }
