{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception
import           Data.Int                   (Int64)
import           Data.Text.Conversions      (convertText)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Clock.POSIX      (getPOSIXTime)

import           Hasura.App
import           Hasura.Logging             (Hasura, LogLevel (..), defaultEnabledEngineLogTypes)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadataFromHdbTables)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Migrate      (downgradeCatalog, dropCatalog)
import           Hasura.Server.Version

import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Environment           as Env
import qualified Database.PG.Query          as Q
import qualified Hasura.Tracing             as Tracing
import qualified System.Exit                as Sys
import qualified System.Metrics             as EKG
import qualified System.Posix.Signals       as Signals


main :: IO ()
main = do
  tryExit $ do
    args <- parseArgs
    env  <- Env.getEnvironment
    runApp env args
  where
    tryExit io = try io >>= \case
      Left (ExitException _code msg) -> BC.putStrLn msg >> Sys.exitFailure
      Right r -> return r

runApp :: Env.Environment -> HGEOptions Hasura -> IO ()
runApp env (HGEOptionsG rci hgeCmd) = do
  initTime <- liftIO getCurrentTime
  globalCtx@GlobalCtx{..} <- initGlobalCtx rci

  withVersion $$(getVersionFromEnvironment) $ case hgeCmd of
    HCServe serveOptions -> do
      serveCtx <- initialiseServeCtx env globalCtx serveOptions

      ekgStore <- liftIO do
        s <- EKG.newStore
        EKG.registerGcMetrics s

        let getTimeMs :: IO Int64
            getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

        EKG.registerCounter "ekg.server_timestamp_ms" getTimeMs s
        pure s

      let shutdownApp = return ()
      -- Catches the SIGTERM signal and initiates a graceful shutdown.
      -- Graceful shutdown for regular HTTP requests is already implemented in
      -- Warp, and is triggered by invoking the 'closeSocket' callback.
      -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C
      -- once again, we terminate the process immediately.
      _ <- liftIO $ Signals.installHandler
        Signals.sigTERM
        (Signals.CatchOnce (shutdownGracefully $ _scShutdownLatch serveCtx))
        Nothing
      flip runPGMetadataStorageApp (_scPgPool serveCtx) $
        runHGEServer env serveOptions serveCtx Nothing initTime shutdownApp Nothing ekgStore

    HCExport -> do
      res <- runTxWithMinimalPool _gcConnInfo fetchMetadataFromHdbTables
      either (printErrJExit MetadataExportError) printJSON res

    HCClean -> do
      res <- runTxWithMinimalPool _gcConnInfo dropCatalog
      let cleanSuccessMsg = "successfully cleaned graphql-engine related data"
      either (printErrJExit MetadataCleanError) (const $ liftIO $ putStrLn cleanSuccessMsg) res

    HCExecute -> do
      queryBs <- liftIO BL.getContents
      let sqlGenCtx = SQLGenCtx False
      pool <- mkMinimalPool _gcConnInfo
      res <- runAsAdmin pool sqlGenCtx _gcHttpManager $ do
        schemaCache <- buildRebuildableSchemaCache env
        execQuery env queryBs
          & Tracing.runTraceTWithReporter Tracing.noReporter "execute"
          & runHasSystemDefinedT (SystemDefined False)
          & runCacheRWT schemaCache
          & fmap (\(res, _, _) -> res)
      either (printErrJExit ExecuteProcessError) (liftIO . BLC.putStrLn) res

    HCDowngrade opts -> do
      res <- runTxWithMinimalPool _gcConnInfo $ downgradeCatalog opts initTime
      either (printErrJExit DowngradeProcessError) (liftIO . print) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ convertText currentVersion
  where
    runTxWithMinimalPool connInfo tx = do
      minimalPool <- mkMinimalPool connInfo
      liftIO $ runExceptT $ Q.runTx minimalPool (Q.ReadCommitted, Nothing) tx

    -- | Generate Postgres pool with single connection.
    -- It is useful when graphql-engine executes a transaction on database
    -- and exits in commands other than 'serve'.
    mkMinimalPool connInfo = do
      pgLogger <- _lsPgLogger <$> mkLoggers defaultEnabledEngineLogTypes LevelInfo
      let connParams = Q.defaultConnParams { Q.cpConns = 1 }
      liftIO $ Q.initPGPool connInfo connParams pgLogger
