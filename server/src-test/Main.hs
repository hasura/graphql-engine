module Main (main) where

import           Hasura.Prelude

import           Control.Concurrent.MVar
import           Control.Natural            ((:~>) (..))
import           Data.Time.Clock            (getCurrentTime)
import           Options.Applicative
import           System.Environment         (getEnvironment)
import           System.Exit                (exitFailure)
import           Test.Hspec

import           Test.QuickCheck


import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Database.PG.Query          as Q
import qualified Network.HTTP.Client        as HTTP
import qualified Network.HTTP.Client.TLS    as HTTP
import qualified Test.Hspec.Runner          as Hspec

import           Hasura.Db                  (PGExecCtx (..))
import           Hasura.RQL.Types           (SQLGenCtx (..), adminUserInfo)
import           Hasura.RQL.Types.Run
import           Hasura.Server.Init         (DowngradeOptions (..),
                                             RawConnInfo, mkConnInfo, mkRawConnInfo,
                                             parseRawConnInfo, runWithEnv)
import           Hasura.Server.Migrate

import qualified Hasura.IncrementalSpec     as IncrementalSpec
import qualified Hasura.RQL.MetadataSpec    as MetadataSpec
import qualified Hasura.Server.MigrateSpec  as MigrateSpec

data TestSuites
  = AllSuites !RawConnInfo
  | SingleSuite !TestSuite

data TestSuite
  = UnitSuite
  | PostgresSuite !RawConnInfo

main :: IO ()
main = parseArgs >>= \case
  AllSuites pgConnOptions -> do
    postgresSpecs <- buildPostgresSpecs pgConnOptions
    runHspec (unitSpecs *> postgresSpecs)
  SingleSuite suite -> case suite of
    UnitSuite                   -> runHspec unitSpecs
    PostgresSuite pgConnOptions -> runHspec =<< buildPostgresSpecs pgConnOptions

unitSpecs :: Spec
unitSpecs = do
  describe "Hasura.Incremental" IncrementalSpec.spec
  describe "Hasura.RQL.Metadata" MetadataSpec.spec

buildPostgresSpecs :: RawConnInfo -> IO Spec
buildPostgresSpecs pgConnOptions = do
  env <- getEnvironment

  rawPGConnInfo <- flip onLeft printErrExit $ runWithEnv env (mkRawConnInfo pgConnOptions)
  pgConnInfo <- flip onLeft printErrExit $ mkConnInfo rawPGConnInfo

  let setupCacheRef = do
        pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams { Q.cpConns = 1 } print

        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        let runContext = RunCtx adminUserInfo httpManager (SQLGenCtx False)
            pgContext = PGExecCtx pgPool Q.Serializable

            runAsAdmin :: Run a -> IO a
            runAsAdmin =
                  peelRun runContext pgContext Q.ReadWrite
              >>> runExceptT
              >=> flip onLeft printErrJExit

        schemaCache <- snd <$> runAsAdmin (migrateCatalog =<< liftIO getCurrentTime)
        cacheRef <- newMVar schemaCache
        pure $ NT (runAsAdmin . flip MigrateSpec.runCacheRefT cacheRef)

  pure $ beforeAll setupCacheRef $
    describe "Hasura.Server.Migrate" $ MigrateSpec.spec pgConnInfo

parseArgs :: IO TestSuites
parseArgs = execParser $ info (helper <*> (parseNoCommand <|> parseSubCommand)) $
  fullDesc <> header "Hasura GraphQL Engine test suite"
  where
<<<<<<< HEAD
    runMigrateTests pgConnOptions = do
      env <- getEnvironment
      rawPGConnInfo <- flip onLeft printErrExit $ runWithEnv env (mkRawConnInfo pgConnOptions)
      pgConnInfo <- flip onLeft printErrExit $ mkConnInfo rawPGConnInfo
      pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams { Q.cpConns = 1 } print

      httpManager <- HTTP.newManager HTTP.tlsManagerSettings
      let runContext = RunCtx adminUserInfo httpManager (SQLGenCtx False)
          pgContext = PGExecCtx pgPool Q.Serializable

          runAsAdmin :: Run a -> IO (Either QErr a)
          runAsAdmin = runExceptT . fmap fst . peelRun emptySchemaCache runContext pgContext Q.ReadWrite

      runHspec $ do
        describe "Hasura.Server.Migrate" $ do
          let dropAndInit time = runAsAdmin $
                dropCatalog *> migrateCatalog Nothing time
              upgradeToLatest time = runAsAdmin $
                migrateCatalog Nothing time
              downgradeTo ver time = runAsAdmin $
                migrateCatalog (Just DowngradeOptions{ dgoDryRun = False, dgoTargetVersion = ver }) time
              dumpSchema = runAsAdmin $
                execPGDump (PGDumpReqBody ["--schema-only"] (Just False)) pgConnInfo

          describe "migrateCatalog" $ do
            it "initializes the catalog" $ do
              (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized

            it "is idempotent" $ do
              time <- getCurrentTime
              dropAndInit time `shouldReturn` Right MRInitialized
              firstDump <- dumpSchema
              firstDump `shouldSatisfy` isRight
              dropAndInit time `shouldReturn` Right MRInitialized
              secondDump <- dumpSchema
              secondDump `shouldBe` firstDump
              
            it "supports upgrades after downgrade to version 12" $ do
              time <- getCurrentTime
              dropAndInit time `shouldReturn` Right MRInitialized
              firstDump <- dumpSchema
              firstDump `shouldSatisfy` isRight
              downgradeResult <- downgradeTo "12" time 
              downgradeResult `shouldSatisfy` \case
                Right MRMigrated{} -> True
                _ -> False
              upgradeToLatest time `shouldReturn` Right (MRMigrated "12")

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

    runPropertyTests = do
      putStrLn "Running property tests"
      passed <- isSuccess <$> quickCheckResult (withMaxSuccess 30 prop_replacemetadata)
      unless passed $ printErrExit "Property tests failed"
      where
        prop_replacemetadata =
          forAll (resize 3 genReplaceMetadata) $ \metadata ->
            let encodedString = encJToBS $ AO.toEncJSON $ replaceMetadataToOrdJSON metadata
                eitherResult :: Either String ReplaceMetadata
                  = eitherDecodeStrict encodedString
            in case eitherResult of
                 Left err -> counterexample err False
                 Right _  -> property True

    runHspec :: Spec -> IO ()
    runHspec m = do
      config <- Hspec.readConfig Hspec.defaultConfig []
      Hspec.evaluateSummary =<< Hspec.runSpec m config

    printErrExit :: String -> IO a
    printErrExit = (*> exitFailure) . putStrLn
=======
    parseNoCommand = AllSuites <$> parseRawConnInfo
    parseSubCommand = fmap SingleSuite . subparser $ mconcat
      [ command "unit" $ info (pure UnitSuite) $
          progDesc "Only run unit tests"
      , command "postgres" $ info (helper <*> (PostgresSuite <$> parseRawConnInfo)) $
          progDesc "Only run Postgres integration tests"
      ]

runHspec :: Spec -> IO ()
runHspec m = do
  config <- Hspec.readConfig Hspec.defaultConfig []
  Hspec.evaluateSummary =<< Hspec.runSpec m config

printErrExit :: String -> IO a
printErrExit = (*> exitFailure) . putStrLn

printErrJExit :: (A.ToJSON a) => a -> IO b
printErrJExit = (*> exitFailure) . BL.putStrLn . A.encode
>>>>>>> origin
