{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception
import           Data.Int                   (Int64)
import           Data.Text.Conversions      (convertText)
import           Data.Time.Clock.POSIX      (getPOSIXTime)

import           Hasura.App
import           Hasura.Logging             (Hasura)
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Migrate      (downgradeCatalog)
import           Hasura.Server.Version

import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Environment           as Env
import qualified Database.PG.Query          as Q
import qualified System.Exit                as Sys
import qualified System.Metrics             as EKG
import qualified System.Posix.Signals       as Signals


main :: IO ()
main = do
  tryExit $ do
    args <- parseArgs
    env  <- Env.getEnvironment
    unAppM (runApp env args)
  where
    tryExit io = try io >>= \case
      Left (ExitException _code msg) -> BC.putStrLn msg >> Sys.exitFailure
      Right r -> return r

runApp :: Env.Environment -> HGEOptions Hasura -> AppM ()
runApp env hgeOptions =
  withVersion $$(getVersionFromEnvironment) $ do
  case hoCommand hgeOptions of
    HCServe serveOptions -> do
      (initCtx, initTime) <- initialiseCtx env hgeOptions

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
        (Signals.CatchOnce (shutdownGracefully initCtx))
        Nothing
      runHGEServer env serveOptions initCtx Nothing initTime shutdownApp Nothing ekgStore

    HCExport -> do
      (InitCtx{..}, _) <- initialiseCtx env hgeOptions
      res <- runTx' _icMetadataPool Q.ReadCommitted getMetadata
      either (printErrJExit MetadataExportError) printJSON res

    HCClean -> do
      (InitCtx{..}, _) <- initialiseCtx env hgeOptions
      res <- runTx' _icMetadataPool Q.ReadCommitted dropHdbCatalogSchema
      either (printErrJExit MetadataCleanError) (const cleanSuccess) res

    HCExecute -> do
      (InitCtx{..}, _) <- initialiseCtx env hgeOptions
      queryBs <- liftIO BL.getContents
      result <- execQuery env _icHttpManager _icDefaultSourceConfig _icMetadata queryBs
      either (printErrJExit ExecuteProcessError) (liftIO . BLC.putStrLn) result

    HCDowngrade opts -> do
      (InitCtx{..}, initTime) <- initialiseCtx env hgeOptions
      res <- runTx' _icMetadataPool Q.ReadCommitted $ downgradeCatalog opts initTime
      either (printErrJExit DowngradeProcessError) (liftIO . print) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ convertText currentVersion
  where
    runTx' pool txIso =
      liftIO . runExceptT . Q.runTx pool (txIso, Nothing)

    cleanSuccess = liftIO $ putStrLn "successfully cleaned graphql-engine related data"
