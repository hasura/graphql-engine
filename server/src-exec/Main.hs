{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Text.Conversions      (convertText)

import           Hasura.App
import           Hasura.Logging             (Hasura)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Migrate      (downgradeCatalog, dropCatalog)
import           Hasura.Server.Version

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Database.PG.Query          as Q
import qualified System.Posix.Signals       as Signals

main :: IO ()
main = parseArgs >>= unAppM . runApp

runApp :: HGEOptions Hasura -> AppM ()
runApp (HGEOptionsG rci hgeCmd) =
  withVersion $$(getVersionFromEnvironment) case hgeCmd of
    HCServe serveOptions -> do
      (initCtx, initTime) <- initialiseCtx hgeCmd rci
      -- Catches the SIGTERM signal and initiates a graceful shutdown.
      -- Graceful shutdown for regular HTTP requests is already implemented in 
      -- Warp, and is triggered by invoking the 'closeSocket' callback.
      -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C 
      -- once again, we terminate the process immediately.
      _ <- liftIO $ Signals.installHandler
        Signals.sigTERM
        (Signals.CatchOnce (shutdownGracefully initCtx))
        Nothing
      runHGEServer serveOptions initCtx initTime
    HCExport -> do
      (initCtx, _) <- initialiseCtx hgeCmd rci
      res <- runTx' initCtx fetchMetadata Q.ReadCommitted
      either printErrJExit printJSON res

    HCClean -> do
      (initCtx, _) <- initialiseCtx hgeCmd rci
      res <- runTx' initCtx dropCatalog Q.ReadCommitted
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      (InitCtx{..}, _) <- initialiseCtx hgeCmd rci
      queryBs <- liftIO BL.getContents
      let sqlGenCtx = SQLGenCtx False
      res <- runAsAdmin _icPgPool sqlGenCtx _icHttpManager do
        schemaCache <- buildRebuildableSchemaCache
        execQuery queryBs
          & runHasSystemDefinedT (SystemDefined False)
          & runCacheRWT schemaCache
          & fmap (\(res, _, _) -> res)
      either printErrJExit (liftIO . BLC.putStrLn) res

    HCDowngrade opts -> do
      (InitCtx{..}, initTime) <- initialiseCtx hgeCmd rci
      let sqlGenCtx = SQLGenCtx False
      res <- downgradeCatalog opts initTime
             & runAsAdmin _icPgPool sqlGenCtx _icHttpManager
      either printErrJExit (liftIO . print) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ convertText currentVersion
  where
    runTx' initCtx tx txIso =
      liftIO $ runExceptT $ Q.runTx (_icPgPool initCtx) (txIso, Nothing) tx

    cleanSuccess = liftIO $ putStrLn "successfully cleaned graphql-engine related data"
