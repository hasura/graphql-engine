module Main (main) where

import           Hasura.Prelude

import           Data.Either             (isRight)
import           Data.Time.Clock         (getCurrentTime)
import           Options.Applicative
import           System.Environment      (getEnvironment)
import           System.Exit             (exitFailure)
import           Test.Hspec

import qualified Database.PG.Query       as Q
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Test.Hspec.Runner       as Hspec

import           Hasura.Db               (PGExecCtx (..))
import           Hasura.RQL.DDL.Metadata (ClearMetadata (..), runClearMetadata)
import           Hasura.RQL.Types        (QErr, SQLGenCtx (..), adminUserInfo,
                                          emptySchemaCache, successMsg)
import           Hasura.Server.Init      (mkConnInfo, mkRawConnInfo,
                                          parseRawConnInfo, runWithEnv)
import           Hasura.Server.Migrate
import           Hasura.Server.PGDump
import           Hasura.Server.Query     (Run, RunCtx (..), peelRun)

main :: IO ()
main = do
  pgConnOptions <- execParser $ info (helper <*> parseRawConnInfo) $
    fullDesc <> header "Hasura GraphQL Engine test suite"
  env <- getEnvironment

  rawPGConnInfo <- flip onLeft printErrExit $ runWithEnv env (mkRawConnInfo pgConnOptions)
  pgConnInfo <- flip onLeft printErrExit $ mkConnInfo rawPGConnInfo
  pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams { Q.cpConns = 1 } print

  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  let runContext = RunCtx adminUserInfo httpManager (SQLGenCtx False)
      pgContext = PGExecCtx pgPool Q.Serializable

      runAsAdmin :: Run a -> IO (Either QErr a)
      runAsAdmin = runExceptT . fmap fst . peelRun emptySchemaCache runContext pgContext

  runHspec $ do
    describe "Hasura.Server.Migrate" $ do
      let dropAndInit time = runAsAdmin (dropCatalog *> migrateCatalog time)

      describe "migrateCatalog" $ do
        it "initializes the catalog" $ do
          (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized

        it "is idempotent" $ do
          let dumpSchema = runAsAdmin $
                execPGDump (PGDumpReqBody ["--schema-only"] (Just False)) pgConnInfo
          time <- getCurrentTime
          dropAndInit time `shouldReturn` Right MRInitialized
          firstDump <- dumpSchema
          firstDump `shouldSatisfy` isRight
          dropAndInit time `shouldReturn` Right MRInitialized
          secondDump <- dumpSchema
          secondDump `shouldBe` firstDump

      describe "recreateSystemMetadata" $ do
        let dumpMetadata = runAsAdmin $
              execPGDump (PGDumpReqBody ["--schema=hdb_catalog"] (Just False)) pgConnInfo

        it "is idempotent" $ do
          (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized
          firstDump <- dumpMetadata
          firstDump `shouldSatisfy` isRight
          runAsAdmin recreateSystemMetadata `shouldReturn` Right ()
          secondDump <- dumpMetadata
          secondDump `shouldBe` firstDump

        it "does not create any objects affected by ClearMetadata" $ do
          (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized
          firstDump <- dumpMetadata
          firstDump `shouldSatisfy` isRight
          runAsAdmin (runClearMetadata ClearMetadata) `shouldReturn` Right successMsg
          secondDump <- dumpMetadata
          secondDump `shouldBe` firstDump

  where
    runHspec :: Spec -> IO ()
    runHspec m = do
      config <- Hspec.readConfig Hspec.defaultConfig []
      Hspec.evaluateSummary =<< Hspec.runSpec m config

    printErrExit :: String -> IO a
    printErrExit = (*> exitFailure) . putStrLn
