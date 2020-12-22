module Main (main) where

import           Hasura.Prelude

import           Control.Concurrent.MVar
import           Control.Natural                     ((:~>) (..))
import           Data.Time.Clock                     (getCurrentTime)
import           Options.Applicative
import           System.Environment                  (getEnvironment)
import           System.Exit                         (exitFailure)
import           Test.Hspec

import qualified Data.Aeson                          as A
import qualified Data.ByteString.Lazy.Char8          as BL
import qualified Data.Environment                    as Env
import qualified Database.PG.Query                   as Q
import qualified Network.HTTP.Client                 as HTTP
import qualified Network.HTTP.Client.TLS             as HTTP
import qualified Test.Hspec.Runner                   as Hspec

import           Hasura.Backends.Postgres.Connection (liftTx, mkPGExecCtx)
import           Hasura.RQL.DDL.Schema.Catalog       (fetchMetadataFromCatalog)
import           Hasura.RQL.Types                    (SQLGenCtx (..), runMetadataT, RemoteSchemaPermsCtx (..))
import           Hasura.RQL.Types.Run
import           Hasura.Server.Init                  (RawConnInfo, mkConnInfo, mkRawConnInfo,
                                                      parseRawConnInfo, runWithEnv)
import           Hasura.Server.Migrate
import           Hasura.Server.Version
import           Hasura.Session                      (adminUserInfo)

import qualified Data.NonNegativeIntSpec             as NonNegetiveIntSpec
import qualified Data.Parser.CacheControlSpec        as CacheControlParser
import qualified Data.Parser.JSONPathSpec            as JsonPath
import qualified Data.Parser.URLTemplate             as URLTemplate
import qualified Data.TimeSpec                       as TimeSpec
import qualified Hasura.IncrementalSpec              as IncrementalSpec
-- import qualified Hasura.RQL.MetadataSpec      as MetadataSpec
import qualified Hasura.CacheBoundedSpec             as CacheBoundedSpec
import qualified Hasura.Server.AuthSpec              as AuthSpec
import qualified Hasura.Server.MigrateSpec           as MigrateSpec
import qualified Hasura.Server.TelemetrySpec         as TelemetrySpec

data TestSuites
  = AllSuites !RawConnInfo
  -- ^ Run all test suites. It probably doesn't make sense to be able to specify additional
  -- hspec args here.
  | SingleSuite ![String] !TestSuite
  -- ^ Args to pass through to hspec (as if from 'getArgs'), and the specific suite to run.

data TestSuite
  = UnitSuite
  | PostgresSuite !RawConnInfo

main :: IO ()
main = withVersion $$(getVersionFromEnvironment) $ parseArgs >>= \case
  AllSuites pgConnOptions -> do
    postgresSpecs <- buildPostgresSpecs pgConnOptions
    runHspec [] (unitSpecs *> postgresSpecs)
  SingleSuite hspecArgs suite -> runHspec hspecArgs =<< case suite of
    UnitSuite                   -> pure unitSpecs
    PostgresSuite pgConnOptions -> buildPostgresSpecs pgConnOptions

unitSpecs :: Spec
unitSpecs = do
  describe "Data.Parser.CacheControl" CacheControlParser.spec
  describe "Data.Parser.URLTemplate" URLTemplate.spec
  describe "Data.Parser.JSONPath" JsonPath.spec
  describe "Hasura.Incremental" IncrementalSpec.spec
  -- describe "Hasura.RQL.Metadata" MetadataSpec.spec -- Commenting until optimizing the test in CI
  describe "Data.Time" TimeSpec.spec
  describe "Data.NonNegativeInt" NonNegetiveIntSpec.spec
  describe "Hasura.Server.Telemetry" TelemetrySpec.spec
  describe "Hasura.Server.Auth" AuthSpec.spec
  describe "Hasura.Cache.Bounded" CacheBoundedSpec.spec

buildPostgresSpecs :: HasVersion => RawConnInfo -> IO Spec
buildPostgresSpecs pgConnOptions = do
  env <- getEnvironment

  rawPGConnInfo <- flip onLeft printErrExit $ runWithEnv env (mkRawConnInfo pgConnOptions)
  pgConnInfo <- flip onLeft printErrExit $ mkConnInfo rawPGConnInfo

  let setupCacheRef = do
        pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams { Q.cpConns = 1 } print
        let pgContext = mkPGExecCtx Q.Serializable pgPool
        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        let runContext = RunCtx adminUserInfo httpManager (SQLGenCtx False) RemoteSchemaPermsDisabled

            runAsAdmin :: RunT IO a -> IO a
            runAsAdmin =
                  peelRun runContext pgContext Q.ReadWrite Nothing
              >>> runExceptT
              >=> flip onLeft printErrJExit

        (schemaCache, metadata) <- runAsAdmin do
          sc <- snd <$> (migrateCatalog (Env.mkEnvironment env) =<< liftIO getCurrentTime)
          metadata <- liftTx fetchMetadataFromCatalog
          pure (sc, metadata)
        cacheRef <- newMVar schemaCache
        pure $ NT (runAsAdmin . flip MigrateSpec.runCacheRefT cacheRef . fmap fst . runMetadataT metadata)

  pure $ beforeAll setupCacheRef $
    describe "Hasura.Server.Migrate" $ MigrateSpec.spec pgConnInfo

parseArgs :: IO TestSuites
parseArgs = execParser $ info (helper <*> (parseNoCommand <|> parseSubCommand)) $
  fullDesc <> header "Hasura GraphQL Engine test suite"
  where
    parseNoCommand = AllSuites <$> parseRawConnInfo
    parseSubCommand = SingleSuite <$> parseHspecPassThroughArgs <*> subCmd
      where
        subCmd = subparser $ mconcat
          [ command "unit" $ info (pure UnitSuite) $
              progDesc "Only run unit tests"
          , command "postgres" $ info (helper <*> (PostgresSuite <$> parseRawConnInfo)) $
              progDesc "Only run Postgres integration tests"
          ]
        -- Add additional arguments and tweak as needed:
        hspecArgs = ["match", "skip"]
        -- parse to a list of arguments as they'd appear from 'getArgs':
        parseHspecPassThroughArgs :: Parser [String]
        parseHspecPassThroughArgs = fmap concat $ for hspecArgs $ \nm->
          fmap (maybe [] (\a -> ["--"<>nm , a])) $ optional $
            strOption ( long nm <>
                        metavar "<PATTERN>" <>
                        help "Flag passed through to hspec (see hspec docs)." )


runHspec :: [String] -> Spec -> IO ()
runHspec hspecArgs m = do
  config <- Hspec.readConfig Hspec.defaultConfig hspecArgs
  Hspec.evaluateSummary =<< Hspec.runSpec m config

printErrExit :: String -> IO a
printErrExit = (*> exitFailure) . putStrLn

printErrJExit :: (A.ToJSON a) => a -> IO b
printErrJExit = (*> exitFailure) . BL.putStrLn . A.encode
