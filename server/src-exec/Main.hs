{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Hasura.Prelude

import qualified Control.Concurrent.Extended as C
import qualified Data.ByteString.Char8       as BC
import qualified Data.Environment            as Env
import qualified Database.PG.Query           as Q
import qualified System.Exit                 as Sys
import qualified System.Metrics              as EKG
import qualified System.Posix.Signals        as Signals

import           Control.Exception
import           Control.Monad.Trans.Managed (ManagedT (..), lowerManagedT)
import           Data.Int                    (Int64)
import           Data.Kind                   (Type)
import           Data.Text.Conversions       (convertText)
import           Data.Time.Clock             (getCurrentTime)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           GHC.TypeLits                (Symbol)

import qualified Hasura.GC                   as GC

import           Hasura.App
import           Hasura.Logging              (Hasura, LogLevel (..), defaultEnabledEngineLogTypes)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Metrics       (ServerMetricsSpec, createServerMetrics)
import           Hasura.Server.Migrate       (downgradeCatalog)
import           Hasura.Server.Version
import           Hasura.Server.Version.TH

main :: IO ()
main = do
  tryExit $ do
    args <- parseArgs
    env  <- Env.getEnvironment
    runApp env args
  where
    tryExit io = try io >>= \case
      Left (ExitException _code msg) -> BC.putStrLn msg >> Sys.exitFailure
      Right r                        -> return r

runApp :: Env.Environment -> HGEOptions Hasura -> IO ()
runApp env (HGEOptionsG rci metadataDbUrl hgeCmd) = do
  initTime <- liftIO getCurrentTime
  globalCtx@GlobalCtx{..} <- initGlobalCtx env metadataDbUrl rci
  let
    (maybeDefaultPgConnInfo, maybeRetries) = _gcDefaultPostgresConnInfo

  withVersion $$(getVersionFromEnvironment) $ case hgeCmd of
    HCServe serveOptions -> do
      (ekgStore, serverMetrics) <- liftIO $ do
        store <- EKG.newStore @AppMetricsSpec
        EKG.register (EKG.subset GcSubset store) EKG.registerGcMetrics

        let getTimeMs :: IO Int64
            getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime
        EKG.register store $ EKG.registerCounter ServerTimestampMs () getTimeMs

        serverMetrics <-
          liftIO $ createServerMetrics $ EKG.subset ServerSubset store

        pure (EKG.subset EKG.emptyOf store, serverMetrics)

      -- It'd be nice if we didn't have to call runManagedT twice here, but
      -- there is a data dependency problem since the call to runPGMetadataStorageApp
      -- below depends on serveCtx.
      runManagedT (initialiseServeCtx env globalCtx serveOptions) $ \serveCtx -> do
        -- Catches the SIGTERM signal and initiates a graceful shutdown.
        -- Graceful shutdown for regular HTTP requests is already implemented in
        -- Warp, and is triggered by invoking the 'closeSocket' callback.
        -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C
        -- once again, we terminate the process immediately.

        -- The function is written in this style to avoid the shutdown
        -- handler retaining a reference to the entire serveCtx (see #344)
        -- If you modify this code then you should check the core to see
        -- that serveCtx is not retained.
        _ <- case serveCtx of
               ServeCtx{_scShutdownLatch} ->
                liftIO $ do
                   void $ Signals.installHandler Signals.sigTERM (Signals.CatchOnce (shutdownGracefully _scShutdownLatch)) Nothing
                   void $ Signals.installHandler Signals.sigINT  (Signals.CatchOnce (shutdownGracefully _scShutdownLatch)) Nothing

        let Loggers _ logger pgLogger = _scLoggers serveCtx

        _idleGCThread <- C.forkImmortal "ourIdleGC" logger $
          GC.ourIdleGC logger (seconds 0.3) (seconds 10) (seconds 60)

        flip runPGMetadataStorageAppT (_scMetadataDbPool serveCtx, pgLogger) . lowerManagedT $ do
          runHGEServer (const $ pure ()) env serveOptions serveCtx initTime Nothing serverMetrics ekgStore

    HCExport -> do
      res <- runTxWithMinimalPool _gcMetadataDbConnInfo fetchMetadataFromCatalog
      either (printErrJExit MetadataExportError) printJSON res

    HCClean -> do
      res <- runTxWithMinimalPool _gcMetadataDbConnInfo dropHdbCatalogSchema
      let cleanSuccessMsg = "successfully cleaned graphql-engine related data"
      either (printErrJExit MetadataCleanError) (const $ liftIO $ putStrLn cleanSuccessMsg) res

    HCDowngrade opts -> do
      let defaultSourceConfig = maybeDefaultPgConnInfo <&> \(dbUrlConf, _) ->
            let pgSourceConnInfo = PostgresSourceConnInfo dbUrlConf
                                   (Just setPostgresPoolSettings{_ppsRetries = maybeRetries <|> Just 1})
                                   False
                                   Q.ReadCommitted
                                   Nothing
            in PostgresConnConfiguration pgSourceConnInfo Nothing
      res <- runTxWithMinimalPool _gcMetadataDbConnInfo $ downgradeCatalog defaultSourceConfig opts initTime
      either (printErrJExit DowngradeProcessError) (liftIO . print) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ convertText currentVersion
  where
    runTxWithMinimalPool connInfo tx = lowerManagedT $ do
      minimalPool <- mkMinimalPool connInfo
      liftIO $ runExceptT $ Q.runTx minimalPool (Q.ReadCommitted, Nothing) tx

    -- | Generate Postgres pool with single connection.
    -- It is useful when graphql-engine executes a transaction on database
    -- and exits in commands other than 'serve'.
    mkMinimalPool connInfo = do
      pgLogger <- _lsPgLogger <$> mkLoggers defaultEnabledEngineLogTypes LevelInfo
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      liftIO $ Q.initPGPool connInfo connParams pgLogger

-- | A specification of all EKG metrics tracked in `runApp`.
data AppMetricsSpec
  :: Symbol -- Metric name
  -> EKG.MetricType -- Metric type, e.g. Counter, Gauge
  -> Type -- Tag structure
  -> Type
  where
  ServerSubset
    :: ServerMetricsSpec name metricType tags
    -> AppMetricsSpec name metricType tags
  GcSubset
    :: EKG.GcMetrics name metricType tags
    -> AppMetricsSpec name metricType tags
  ServerTimestampMs
    :: AppMetricsSpec "ekg.server_timestamp_ms" 'EKG.CounterType ()
