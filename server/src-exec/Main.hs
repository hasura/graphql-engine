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
  (HGEOptionsG rci hgeCmd) <- parseArgs
  case hgeCmd of
    HCServe serveOptions -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog
      runHGEServer serveOptions initCtx Nothing Nothing Nothing
    HCExport -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog
      res <- runTx' initCtx fetchMetadata
      either printErrJExit printJSON res

    HCClean -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog
      res <- runTx' initCtx cleanCatalog
      either printErrJExit (const cleanSuccess) res

    HCExecute -> do
      initCtx <- initialiseCtx hgeCmd rci Nothing mkHttpLog
      queryBs <- BL.getContents
      let sqlGenCtx = SQLGenCtx False
      res <- runAsAdmin (_icPgPool initCtx) sqlGenCtx $ execQuery queryBs
      either printErrJExit BLC.putStrLn res

    HCVersion -> putStrLn $ "Hasura GraphQL Engine: " ++ T.unpack currentVersion
  where
    runTx' :: InitCtx b -> Q.TxE QErr a -> IO (Either QErr a)
    runTx' initCtx tx =
      runExceptT $ Q.runTx (_icPgPool initCtx) (Q.Serializable, Nothing) tx

    cleanSuccess =
      putStrLn "successfully cleaned graphql-engine related data"
