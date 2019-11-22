{-# LANGUAGE RecordWildCards #-}

module Main where

import           Hasura.App
import           Hasura.Logging             (OSS)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Migrate      (dropCatalog)
import           Hasura.Server.Version

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q

main :: IO ()
main = parseArgs >>= unAppM . runApp

runApp :: HGEOptions OSS -> AppM ()
runApp (HGEOptionsG rci hgeCmd) =
  case hgeCmd of
    HCServe serveOptions -> do
      initCtx <- initialiseCtx hgeCmd rci
      runHGEServer serveOptions initCtx

    HCExport -> do
      initCtx <- initialiseCtx hgeCmd rci
      res <- runTx' initCtx fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      initCtx <- initialiseCtx hgeCmd rci
      res <- runTx' initCtx dropCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      InitCtx{..} <- initialiseCtx hgeCmd rci
      queryBs <- liftIO BL.getContents
      let sqlGenCtx = SQLGenCtx False
      res <- execQuery queryBs
             & runHasSystemDefinedT (SystemDefined False)
             & runAsAdmin _icPgPool sqlGenCtx _icHttpManager
      either printErrJExit (liftIO . BLC.putStrLn) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion

  where
    runTx' initCtx tx =
      liftIO $ runExceptT $ Q.runTx (_icPgPool initCtx) (Q.Serializable, Nothing) tx

-- =======
--     runTx pool tx =
--       runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

--     runTx' pgLogger ci tx = do
--       pool <- getMinimalPool pgLogger ci
--       runExceptT $ Q.runTx pool (Q.Serializable, Nothing) tx

--     runAsAdmin pool sqlGenCtx httpManager m = do
--       res <- runExceptT $ peelRun emptySchemaCache
--        (RunCtx adminUserInfo httpManager sqlGenCtx)
--        (PGExecCtx pool Q.Serializable) Q.ReadWrite m
--       return $ fmap fst res

--     procConnInfo rci =
--       either (printErrExit . connInfoErrModifier) return $
--         mkConnInfo rci

--     getMinimalPool pgLogger ci = do
--       let connParams = Q.defaultConnParams { Q.cpConns = 1 }
--       Q.initPGPool ci connParams pgLogger

--     initialise pool sqlGenCtx (Logger logger) httpMgr = do
--       currentTime <- getCurrentTime
--       -- migrate catalog if necessary
--       migRes <- runAsAdmin pool sqlGenCtx httpMgr $ migrateCatalog currentTime
--       either printErrJExit logger migRes

--       -- retrieve database id
--       eDbId <- runTx pool getDbId
--       either printErrJExit return eDbId

--     prepareEvents pool (Logger logger) = do
--       logger $ mkGenericStrLog LevelInfo "event_triggers" "preparing data"
--       res <- runTx pool unlockAllEvents
--       either printErrJExit return res

--     execQuery queryBs = do
--       query <- case A.decode queryBs of
--         Just jVal -> decodeValue jVal
--         Nothing   -> throw400 InvalidJSON "invalid json"
--       buildSchemaCacheStrict
--       encJToLBS <$> runQueryM query

--     getFromEnv :: (Read a) => a -> String -> IO a
--     getFromEnv defaults env = do
--       mEnv <- lookupEnv env
--       let mRes = case mEnv of
--             Nothing  -> Just defaults
--             Just val -> readMaybe val
--           eRes = maybe (Left $ "Wrong expected type for environment variable: " <> env) Right mRes
--       either printErrExit return eRes

--     cleanSuccess =
--       putStrLn "successfully cleaned graphql-engine related data"

--     -- | Catches the SIGTERM signal and initiates a graceful shutdown. Graceful shutdown for regular HTTP
--     -- requests is already implemented in Warp, and is triggered by invoking the 'closeSocket' callback.
--     -- We only catch the SIGTERM signal once, that is, if the user hits CTRL-C once again, we terminate
--     -- the process immediately.
--     shutdownHandler :: Logger -> IO () -> IO () -> IO ()
--     shutdownHandler (Logger logger) shutdownApp closeSocket =
--       void $ Signals.installHandler
--         Signals.sigTERM
--         (Signals.CatchOnce shutdownSequence)
--         Nothing
--      where
--       shutdownSequence = do
--         closeSocket
--         shutdownApp
--         logShutdown

--       logShutdown = logger $
--         mkGenericStrLog LevelInfo "server" "gracefully shutting down server"
-- >>>>>>> master

    cleanSuccess = liftIO $ putStrLn "successfully cleaned graphql-engine related data"
