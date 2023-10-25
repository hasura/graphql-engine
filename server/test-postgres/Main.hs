{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Constants qualified
import Control.Concurrent.MVar
import Control.Monad.Trans.Managed (lowerManagedT)
import Control.Natural ((:~>) (..))
import Data.Aeson qualified as J
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.Environment qualified as Env
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.URL.Template
import Database.PG.Query qualified as PG
import Hasura.App
  ( AppM,
    BasicConnectionInfo (..),
    initMetadataConnectionInfo,
    initialiseAppEnv,
    mkMSSQLSourceResolver,
    mkPgSourceResolver,
    runAppM,
  )
import Hasura.Backends.Postgres.Connection.Settings
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Base.Error
import Hasura.GraphQL.Schema.Common
import Hasura.Logging
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata (emptyMetadataDefaults)
import Hasura.RQL.Types.ResizePool
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Server.Init
import Hasura.Server.Metrics (ServerMetricsSpec, createServerMetrics)
import Hasura.Server.Migrate
import Hasura.Server.Prometheus (makeDummyPrometheusMetrics)
import Hasura.Server.Types
import Hasura.Tracing (sampleAlways)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import System.Metrics qualified as EKG
import Test.Hasura.EventTriggerCleanupSuite qualified as EventTriggerCleanupSuite
import Test.Hasura.Server.MigrateSuite qualified as MigrateSuite
import Test.Hasura.StreamingSubscriptionSuite qualified as StreamingSubscriptionSuite
import Test.Hspec

{-# ANN main ("HLINT: ignore avoid getEnvironment" :: String) #-}
main :: IO ()
main = do
  env <- getEnvironment
  let envMap = Env.mkEnvironment env

  pgUrlText <- flip onLeft printErrExit
    $ runWithEnv env
    $ do
      let envVar = _envVar databaseUrlOption
      maybeV <- considerEnv envVar
      onNothing maybeV
        $ throwError
        $ "Expected: "
        <> envVar

  let pgConnInfo = PG.ConnInfo 1 $ PG.CDDatabaseURI $ txtToBs pgUrlText
      urlConf = UrlValue $ InputWebhook $ mkPlainTemplate pgUrlText
      sourceConnInfo =
        PostgresSourceConnInfo urlConf (Just setPostgresPoolSettings) True PG.ReadCommitted Nothing
      sourceConfig = PostgresConnConfiguration sourceConnInfo Nothing defaultPostgresExtensionsSchema Nothing mempty
      rci =
        PostgresConnInfo
          { _pciDatabaseConn = Nothing,
            _pciRetries = Nothing
          }
      serveOptions = Constants.serveOptions
      metadataDbUrl = Just (T.unpack pgUrlText)

  pgPool <- PG.initPGPool pgConnInfo J.Null PG.defaultConnParams {PG.cpConns = 1} print
  let pgContext = mkPGExecCtx PG.Serializable pgPool NeverResizePool

      logger :: Logger Hasura = Logger $ \l -> do
        let (logLevel, logType :: EngineLogType Hasura, logDetail) = toEngineLog l
        t <- liftIO $ getFormattedTime Nothing
        liftIO $ putStrLn $ LBS.toString $ J.encode $ EngineLog t logLevel logType logDetail Nothing Nothing

      setupCacheRef = do
        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        metadataConnectionInfo <- initMetadataConnectionInfo envMap metadataDbUrl rci
        let globalCtx = BasicConnectionInfo metadataConnectionInfo Nothing
        (_, serverMetrics) <-
          liftIO $ do
            store <- EKG.newStore @TestMetricsSpec
            serverMetrics <-
              liftIO $ createServerMetrics $ EKG.subset ServerSubset store
            pure (EKG.subset EKG.emptyOf store, serverMetrics)
        prometheusMetrics <- makeDummyPrometheusMetrics
        let sqlGenCtx =
              SQLGenCtx
                Options.Don'tStringifyNumbers
                Options.Don'tDangerouslyCollapseBooleans
                Options.RemoteForwardAccurately
                Options.Don'tOptimizePermissionFilters
                Options.EnableBigQueryStringNumericInput
            maintenanceMode = MaintenanceModeDisabled
            readOnlyMode = ReadOnlyModeDisabled
            staticConfig =
              CacheStaticConfig
                maintenanceMode
                EventingEnabled
                readOnlyMode
                logger
                (const False)
                False
            dynamicConfig =
              CacheDynamicConfig
                Options.InferFunctionPermissions
                Options.DisableRemoteSchemaPermissions
                sqlGenCtx
                mempty
                (_default defaultNamingConventionOption)
                emptyMetadataDefaults
                ApolloFederationDisabled
                (_default closeWebsocketsOnMetadataChangeOption)
                (SchemaSampledFeatureFlags [])
            cacheBuildParams = CacheBuildParams httpManager (mkPgSourceResolver print) mkMSSQLSourceResolver staticConfig

        (_appInit, appEnv) <-
          lowerManagedT
            $ initialiseAppEnv
              envMap
              globalCtx
              serveOptions
              Nothing
              serverMetrics
              prometheusMetrics
              sampleAlways

        let run :: ExceptT QErr AppM a -> IO a
            run =
              runExceptT
                >>> runAppM appEnv
                >>> flip onLeftM printErrJExit

        -- why are we building the schema cache here? it's already built in initialiseContext
        (metadata, schemaCache) <- run do
          metadataWithVersion <-
            snd
              <$> (liftEitherM . runExceptT . _pecRunTx pgContext (PGExecCtxInfo (Tx PG.ReadWrite Nothing) InternalRawQuery))
                (migrateCatalog (Just sourceConfig) defaultPostgresExtensionsSchema maintenanceMode =<< liftIO getCurrentTime)
          schemaCache <- runCacheBuild cacheBuildParams $ buildRebuildableSchemaCache logger envMap metadataWithVersion dynamicConfig Nothing
          pure (_mwrvMetadata metadataWithVersion, schemaCache)

        cacheRef <- newMVar schemaCache
        pure $ NT (run . flip MigrateSuite.runCacheRefT (dynamicConfig, cacheRef) . fmap fst . runMetadataT metadata emptyMetadataDefaults)

  streamingSubscriptionSuite <- StreamingSubscriptionSuite.buildStreamingSubscriptionSuite
  eventTriggerLogCleanupSuite <- EventTriggerCleanupSuite.buildEventTriggerCleanupSuite

  hspec do
    describe "Migrate suite"
      $ beforeAll setupCacheRef
      $ describe "Hasura.Server.Migrate"
      $ MigrateSuite.suite sourceConfig pgContext pgConnInfo
    describe "Streaming subscription suite" $ streamingSubscriptionSuite
    describe "Event trigger log cleanup suite" $ eventTriggerLogCleanupSuite

printErrExit :: String -> IO a
printErrExit = (*> exitFailure) . putStrLn

printErrJExit :: (J.ToJSON a) => a -> IO b
printErrJExit = (*> exitFailure) . BL.putStrLn . J.encode

-- | Used only for 'runApp' above.
data TestMetricsSpec name metricType tags
  = ServerSubset (ServerMetricsSpec name metricType tags)
