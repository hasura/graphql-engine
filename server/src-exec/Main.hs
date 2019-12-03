{-# LANGUAGE RecordWildCards #-}

module Main where

import           Hasura.App
import           Hasura.Logging             (Hasura)
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

runApp :: HGEOptions Hasura -> AppM ()
runApp (HGEOptionsG rci hgeCmd) =
  case hgeCmd of
    HCServe serveOptions -> do
      (initCtx, initTime) <- initialiseCtx hgeCmd rci
      runHGEServer serveOptions initCtx initTime
    HCExport -> do
      (initCtx, _) <- initialiseCtx hgeCmd rci
      res <- runTx' initCtx fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      (initCtx, _) <- initialiseCtx hgeCmd rci
      res <- runTx' initCtx dropCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      (InitCtx{..}, _) <- initialiseCtx hgeCmd rci
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

    cleanSuccess = liftIO $ putStrLn "successfully cleaned graphql-engine related data"
