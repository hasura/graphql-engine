{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Natural ((:~>) (..))
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.Environment qualified as Env
import Data.Time.Clock (getCurrentTime)
import Data.URL.Template
import Database.PG.Query qualified as PG
import Hasura.App
  ( PGMetadataStorageAppT (..),
    mkMSSQLSourceResolver,
    mkPgSourceResolver,
  )
import Hasura.Backends.Postgres.Connection.Settings
import Hasura.Backends.Postgres.Execute.Types
import Hasura.EventTriggerCleanupSuite qualified as EventTriggerCleanupSuite
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata (emptyMetadataDefaults)
import Hasura.RQL.Types.ResizePool
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Server.Init
import Hasura.Server.Init.FeatureFlag as FF
import Hasura.Server.Migrate
import Hasura.Server.MigrateSuite qualified as MigrateSuite
import Hasura.Server.Types
import Hasura.StreamingSubscriptionSuite qualified as StreamingSubscriptionSuite
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import Test.Hspec

main :: IO ()
main = do
  env <- getEnvironment
  let envMap = Env.mkEnvironment env

  pgUrlText <- flip onLeft printErrExit $
    runWithEnv env $ do
      let envVar = _envVar databaseUrlOption
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $
          "Expected: " <> envVar

  let pgConnInfo = PG.ConnInfo 1 $ PG.CDDatabaseURI $ txtToBs pgUrlText
      urlConf = UrlValue $ InputWebhook $ mkPlainURLTemplate pgUrlText
      sourceConnInfo =
        PostgresSourceConnInfo urlConf (Just setPostgresPoolSettings) True PG.ReadCommitted Nothing
      sourceConfig = PostgresConnConfiguration sourceConnInfo Nothing defaultPostgresExtensionsSchema Nothing mempty

  pgPool <- PG.initPGPool pgConnInfo PG.defaultConnParams {PG.cpConns = 1} print
  let pgContext = mkPGExecCtx PG.Serializable pgPool NeverResizePool

      logger :: Logger Hasura = Logger $ \l -> do
        let (logLevel, logType :: EngineLogType Hasura, logDetail) = toEngineLog l
        t <- liftIO $ getFormattedTime Nothing
        liftIO $ putStrLn $ LBS.toString $ A.encode $ EngineLog t logLevel logType logDetail

      setupCacheRef = do
        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        let sqlGenCtx =
              SQLGenCtx
                Options.Don'tStringifyNumbers
                Options.Don'tDangerouslyCollapseBooleans
                Options.Don'tOptimizePermissionFilters
                Options.EnableBigQueryStringNumericInput
            maintenanceMode = MaintenanceModeDisabled
            readOnlyMode = ReadOnlyModeDisabled
            serverConfigCtx =
              ServerConfigCtx
                Options.InferFunctionPermissions
                Options.DisableRemoteSchemaPermissions
                sqlGenCtx
                maintenanceMode
                mempty
                EventingEnabled
                readOnlyMode
                Nothing -- We are not testing the naming convention here, so defaulting to hasura-default
                emptyMetadataDefaults
                (FF.checkFeatureFlag mempty)
            cacheBuildParams = CacheBuildParams httpManager (mkPgSourceResolver print) mkMSSQLSourceResolver serverConfigCtx
            pgLogger = print

            run :: MetadataStorageT (PGMetadataStorageAppT CacheBuild) a -> IO a
            run =
              runMetadataStorageT
                >>> flip runPGMetadataStorageAppT (pgPool, pgLogger)
                >>> runCacheBuild cacheBuildParams
                >>> runExceptT
                >=> flip onLeft printErrJExit
                >=> flip onLeft printErrJExit

        (metadata, schemaCache) <- run do
          metadata <-
            snd
              <$> (liftEitherM . runExceptT . _pecRunTx pgContext (PGExecCtxInfo (Tx PG.ReadWrite Nothing) InternalRawQuery))
                (migrateCatalog (Just sourceConfig) defaultPostgresExtensionsSchema maintenanceMode =<< liftIO getCurrentTime)
          schemaCache <- lift $ lift $ buildRebuildableSchemaCache logger envMap metadata
          pure (metadata, schemaCache)

        cacheRef <- newMVar schemaCache
        pure $ NT (run . flip MigrateSuite.runCacheRefT cacheRef . fmap fst . runMetadataT metadata emptyMetadataDefaults)

  streamingSubscriptionSuite <- StreamingSubscriptionSuite.buildStreamingSubscriptionSuite
  eventTriggerLogCleanupSuite <- EventTriggerCleanupSuite.buildEventTriggerCleanupSuite

  hspec do
    describe "Migrate suite" $
      beforeAll setupCacheRef $
        describe "Hasura.Server.Migrate" $
          MigrateSuite.suite sourceConfig pgContext pgConnInfo
    describe "Streaming subscription suite" $ streamingSubscriptionSuite
    describe "Event trigger log cleanup suite" $ eventTriggerLogCleanupSuite

printErrExit :: String -> IO a
printErrExit = (*> exitFailure) . putStrLn

printErrJExit :: (A.ToJSON a) => a -> IO b
printErrJExit = (*> exitFailure) . BL.putStrLn . A.encode
