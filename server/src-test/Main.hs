module Main (main) where

import           Hasura.Prelude

import           Control.Concurrent.MVar
import           Control.Natural                    ((:~>) (..))
import           Data.Time.Clock                    (getCurrentTime)
import           Data.URL.Template
import           Options.Applicative
import           System.Environment                 (getEnvironment)
import           System.Exit                        (exitFailure)
import           Test.Hspec

import qualified Data.Aeson                         as A
import qualified Data.ByteString.Lazy.Char8         as BL
import qualified Data.Environment                   as Env
import qualified Database.PG.Query                  as Q
import qualified Network.HTTP.Client                as HTTP
import qualified Network.HTTP.Client.TLS            as HTTP
import qualified Test.Hspec.Runner                  as Hspec

import           Hasura.RQL.DDL.Schema.Cache
import           Hasura.RQL.DDL.Schema.Cache.Common
import           Hasura.RQL.DDL.Schema.Source
import           Hasura.RQL.Types
import           Hasura.Server.Init
import           Hasura.Server.Migrate
import           Hasura.Server.Types
import           Hasura.Server.Version

import qualified Data.NonNegativeIntSpec            as NonNegetiveIntSpec
import qualified Data.Parser.CacheControlSpec       as CacheControlParser
import qualified Data.Parser.JSONPathSpec           as JsonPath
import qualified Data.Parser.URLTemplate            as URLTemplate
import qualified Data.TimeSpec                      as TimeSpec
import qualified Hasura.IncrementalSpec             as IncrementalSpec
-- import qualified Hasura.RQL.MetadataSpec      as MetadataSpec
import qualified Hasura.CacheBoundedSpec            as CacheBoundedSpec
import qualified Hasura.RQL.Types.EndpointSpec      as EndpointSpec
import qualified Hasura.SQL.WKTSpec                 as WKTSpec
import qualified Hasura.Server.AuthSpec             as AuthSpec
import qualified Hasura.Server.MigrateSpec          as MigrateSpec
import qualified Hasura.Server.TelemetrySpec        as TelemetrySpec

data TestSuites
  = AllSuites !(Maybe URLTemplate)
  -- ^ Run all test suites. It probably doesn't make sense to be able to specify additional
  -- hspec args here.
  | SingleSuite ![String] !TestSuite
  -- ^ Args to pass through to hspec (as if from 'getArgs'), and the specific suite to run.

data TestSuite
  = UnitSuite
  | PostgresSuite !(Maybe URLTemplate)

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
  describe "Hasura.RQL.Types.Endpoint" EndpointSpec.spec
  describe "Hasura.SQL.WKT" WKTSpec.spec

buildPostgresSpecs :: HasVersion => Maybe URLTemplate -> IO Spec
buildPostgresSpecs maybeUrlTemplate = do
  env <- getEnvironment
  let envMap = Env.mkEnvironment env

  pgUrlTemplate <- flip onLeft printErrExit $ runWithEnv env $ do
                   let envVar = fst databaseUrlEnv
                   maybeV <- withEnv maybeUrlTemplate envVar
                   onNothing maybeV $ throwError $
                               "Expected: --database-url or " <> envVar

  pgUrlText <- flip onLeft printErrExit $ renderURLTemplate envMap pgUrlTemplate
  let pgConnInfo = Q.ConnInfo 1 $ Q.CDDatabaseURI $ txtToBs pgUrlText
      urlConf = UrlValue $ InputWebhook pgUrlTemplate
      sourceConnInfo = PostgresSourceConnInfo urlConf (Just setPostgresPoolSettings) True
      sourceConfig = PostgresConnConfiguration sourceConnInfo Nothing

  pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams { Q.cpConns = 1 } print
  let pgContext = mkPGExecCtx Q.Serializable pgPool

      setupCacheRef = do
        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        let sqlGenCtx = SQLGenCtx False False
            maintenanceMode = MaintenanceModeDisabled
            serverConfigCtx =
              ServerConfigCtx FunctionPermissionsInferred RemoteSchemaPermsDisabled sqlGenCtx maintenanceMode mempty
            cacheBuildParams = CacheBuildParams httpManager (mkPgSourceResolver print) serverConfigCtx

            run :: CacheBuild a -> IO a
            run =
              runCacheBuild cacheBuildParams
              >>> runExceptT
              >=> flip onLeft printErrJExit

        (metadata, schemaCache) <- run do
          metadata <- snd <$> (liftEitherM . runExceptT . runLazyTx pgContext Q.ReadWrite)
                      (migrateCatalog (Just sourceConfig) maintenanceMode =<< liftIO getCurrentTime)
          schemaCache <- buildRebuildableSchemaCache envMap metadata
          pure (metadata, schemaCache)

        cacheRef <- newMVar schemaCache
        pure $ NT (run . flip MigrateSpec.runCacheRefT cacheRef . fmap fst . runMetadataT metadata)

  pure $ beforeAll setupCacheRef $
    describe "Hasura.Server.Migrate" $ MigrateSpec.spec sourceConfig pgContext pgConnInfo

parseArgs :: IO TestSuites
parseArgs = execParser $ info (helper <*> (parseNoCommand <|> parseSubCommand)) $
  fullDesc <> header "Hasura GraphQL Engine test suite"
  where
    parseDbUrlTemplate =
      parseDatabaseUrl <|> (fmap rawConnDetailsToUrl <$> parseRawConnDetails)
    parseNoCommand = AllSuites <$> parseDbUrlTemplate
    parseSubCommand = SingleSuite <$> parseHspecPassThroughArgs <*> subCmd
      where
        subCmd = subparser $ mconcat
          [ command "unit" $ info (pure UnitSuite) $
              progDesc "Only run unit tests"
          , command "postgres" $ info (helper <*> (PostgresSuite <$> parseDbUrlTemplate)) $
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
