module Main
  ( main,
  )
where

import Control.Concurrent.Extended qualified as C
import Control.Exception
import Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import Data.ByteString.Char8 qualified as BC
import Data.Environment qualified as Env
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Text.Conversions (convertText)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PG.Query qualified as PG
import GHC.Debug.Stub
import GHC.TypeLits (Symbol)
import Hasura.App
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Connection.Settings
import Hasura.GC qualified as GC
import Hasura.Logging (Hasura, LogLevel (..), defaultEnabledEngineLogTypes)
import Hasura.Prelude
import Hasura.RQL.DDL.Schema
import Hasura.Server.App (Loggers (..), ServerCtx (..))
import Hasura.Server.Init
import Hasura.Server.Init.FeatureFlag qualified as FeatureFlag
import Hasura.Server.Metrics (ServerMetricsSpec, createServerMetrics)
import Hasura.Server.Migrate (downgradeCatalog)
import Hasura.Server.Prometheus (makeDummyPrometheusMetrics)
import Hasura.Server.Version
import Hasura.ShutdownLatch
import Hasura.Tracing (sampleAlways)
import System.Environment (lookupEnv)
import System.Exit qualified as Sys
import System.Metrics qualified as EKG
import System.Posix.Signals qualified as Signals

main :: IO ()
main = maybeWithGhcDebug $ do
  catch
    do
      args <- parseArgs
      env <- Env.getEnvironment
      runApp env args
    (\(ExitException _code msg) -> BC.putStrLn msg >> Sys.exitFailure)

runApp :: Env.Environment -> HGEOptions (ServeOptions Hasura) -> IO ()
runApp env (HGEOptions rci metadataDbUrl hgeCmd) = do
  initTime <- liftIO getCurrentTime

  case hgeCmd of
    HCServe serveOptions -> do
      globalCtx@GlobalCtx {} <- initGlobalCtx env metadataDbUrl rci
      (ekgStore, serverMetrics) <- liftIO $ do
        store <- EKG.newStore @AppMetricsSpec
        void $ EKG.register (EKG.subset GcSubset store) EKG.registerGcMetrics

        let getTimeMs :: IO Int64
            getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime
        void $ EKG.register store $ EKG.registerCounter ServerTimestampMs () getTimeMs

        serverMetrics <-
          liftIO $ createServerMetrics $ EKG.subset ServerSubset store

        pure (EKG.subset EKG.emptyOf store, serverMetrics)

      prometheusMetrics <- makeDummyPrometheusMetrics

      -- It'd be nice if we didn't have to call runManagedT twice here, but
      -- there is a data dependency problem since the call to runPGMetadataStorageApp
      -- below depends on serverCtx.
      runManagedT (initialiseServerCtx env globalCtx serveOptions Nothing serverMetrics prometheusMetrics sampleAlways (FeatureFlag.checkFeatureFlag env)) $ \serverCtx@ServerCtx {..} -> do
        -- Catches the SIGTERM signal and initiates a graceful shutdown.
        -- Graceful shutdown for regular HTTP requests is already implemented in
        -- Warp, and is triggered by invoking the 'closeSocket' callback.
        -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C
        -- once again, we terminate the process immediately.

        liftIO $ do
          void $ Signals.installHandler Signals.sigTERM (Signals.CatchOnce (shutdownGracefully scShutdownLatch)) Nothing
          void $ Signals.installHandler Signals.sigINT (Signals.CatchOnce (shutdownGracefully scShutdownLatch)) Nothing

        let Loggers _ logger pgLogger = scLoggers

        _idleGCThread <-
          C.forkImmortal "ourIdleGC" logger $
            GC.ourIdleGC logger (seconds 0.3) (seconds 10) (seconds 60)

        flip runPGMetadataStorageAppT (scMetadataDbPool, pgLogger) . lowerManagedT $ do
          runHGEServer (const $ pure ()) env serveOptions serverCtx initTime Nothing ekgStore (FeatureFlag.checkFeatureFlag env)
    HCExport -> do
      GlobalCtx {..} <- initGlobalCtx env metadataDbUrl rci
      res <- runTxWithMinimalPool _gcMetadataDbConnInfo fetchMetadataFromCatalog
      either (throwErrJExit MetadataExportError) printJSON res
    HCClean -> do
      GlobalCtx {..} <- initGlobalCtx env metadataDbUrl rci
      res <- runTxWithMinimalPool _gcMetadataDbConnInfo dropHdbCatalogSchema
      let cleanSuccessMsg = "successfully cleaned graphql-engine related data"
      either (throwErrJExit MetadataCleanError) (const $ liftIO $ putStrLn cleanSuccessMsg) res
    HCDowngrade opts -> do
      GlobalCtx {..} <- initGlobalCtx env metadataDbUrl rci
      let (maybeDefaultPgConnInfo, maybeRetries) = _gcDefaultPostgresConnInfo
      let defaultSourceConfig =
            maybeDefaultPgConnInfo <&> \(dbUrlConf, _) ->
              let pgSourceConnInfo =
                    PostgresSourceConnInfo
                      dbUrlConf
                      (Just setPostgresPoolSettings {_ppsRetries = maybeRetries <|> Just 1})
                      False
                      PG.ReadCommitted
                      Nothing
               in PostgresConnConfiguration pgSourceConnInfo Nothing defaultPostgresExtensionsSchema Nothing mempty
      res <- runTxWithMinimalPool _gcMetadataDbConnInfo $ downgradeCatalog defaultSourceConfig opts initTime
      either (throwErrJExit DowngradeProcessError) (liftIO . print) res
    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ convertText currentVersion
  where
    runTxWithMinimalPool connInfo tx = lowerManagedT $ do
      minimalPool <- mkMinimalPool connInfo
      liftIO $ runExceptT $ PG.runTx minimalPool (PG.ReadCommitted, Nothing) tx

    mkMinimalPool connInfo = do
      pgLogger <- _lsPgLogger <$> mkLoggers defaultEnabledEngineLogTypes LevelInfo
      let connParams = PG.defaultConnParams {PG.cpConns = 1}
      liftIO $ PG.initPGPool connInfo connParams pgLogger

-- | A specification of all EKG metrics tracked in `runApp`.
data
  AppMetricsSpec ::
    Symbol -> -- Metric name
    EKG.MetricType -> -- Metric type, e.g. Counter, Gauge
    Type -> -- Tag structure
    Type
  where
  ServerSubset ::
    ServerMetricsSpec name metricType tags ->
    AppMetricsSpec name metricType tags
  GcSubset ::
    EKG.GcMetrics name metricType tags ->
    AppMetricsSpec name metricType tags
  ServerTimestampMs ::
    AppMetricsSpec "ekg.server_timestamp_ms" 'EKG.CounterType ()

-- | 'withGhcDebug' but conditional on the environment variable
-- @HASURA_GHC_DEBUG=true@. When this is set a debug socket will be opened,
-- otherwise the server will start normally.  This must only be called once and
-- it's argument should be the program's @main@
maybeWithGhcDebug :: IO a -> IO a
maybeWithGhcDebug theMain = do
  lookupEnv "HASURA_GHC_DEBUG" >>= \case
    Just "true" -> do
      putStrLn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      putStrLn "!!!!!    Opening a ghc-debug socket    !!!!!"
      putStrLn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
      withGhcDebug theMain
    _ -> theMain
