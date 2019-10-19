module Main (main) where

import           Hasura.Prelude

import           Data.Time.Clock         (getCurrentTime)
import           Options.Applicative
import           System.Environment      (getEnvironment)
import           System.Exit             (exitFailure)
import           Test.Hspec

import qualified Database.PG.Query       as Q
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Test.Hspec.Core.Runner  as Hspec

import           Hasura.Db               (PGExecCtx (..))
import           Hasura.RQL.Types        (QErr, SQLGenCtx (..), adminUserInfo,
                                          emptySchemaCache)
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
          dropAndInit time `shouldReturn` Right MRInitialized
          secondDump <- dumpSchema
          firstDump `shouldBe` secondDump

      describe "recreateSystemMetadata" $ do
        it "is idempotent" $ do
          let dumpMetadata = runAsAdmin $
                execPGDump (PGDumpReqBody ["--schema=hdb_catalog"] (Just False)) pgConnInfo
          (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized
          firstDump <- dumpMetadata
          runAsAdmin recreateSystemMetadata `shouldReturn` Right ()
          secondDump <- dumpMetadata
          firstDump `shouldBe` secondDump

  where
    runHspec :: Spec -> IO ()
    runHspec = Hspec.evaluateSummary <=< flip Hspec.runSpec Hspec.defaultConfig

    printErrExit :: String -> IO a
    printErrExit = (*> exitFailure) . putStrLn
