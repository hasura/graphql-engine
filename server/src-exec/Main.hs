module Main where

import           Hasura.App
import           Hasura.App.Ops             (cleanCatalog, execQuery)
import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata    (fetchMetadata)
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Logging      (mkHttpLog)
import           Hasura.Server.Version

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q

main :: IO ()
main =  do
  args <- parseArgs
  unAppM $ runApp args

runApp :: HGEOptions -> AppM ()
runApp (HGEOptionsG rci hgeCmd) =
  case hgeCmd of
    HCServe serveOptions -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog Nothing
      runHGEServer serveOptions initCtx Nothing Nothing Nothing unAppM
    HCExport -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog Nothing
      res <- runTx' initCtx fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog Nothing
      res <- runTx' initCtx cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog Nothing
      queryBs <- liftIO BL.getContents
      let sqlGenCtx = SQLGenCtx False
      res <- runAsAdmin (_icPgPool initCtx) sqlGenCtx $ execQuery queryBs
      either printErrJExit (liftIO . BLC.putStrLn) res

    HCVersion -> liftIO $ putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where
    runTx' initCtx tx =
      liftIO $ runExceptT $ Q.runTx (_icPgPool initCtx) (Q.Serializable, Nothing) tx

    cleanSuccess =
      liftIO $ putStrLn "successfully cleaned graphql-engine related data"
