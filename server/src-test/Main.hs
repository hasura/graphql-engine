{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Natural ((:~>) (..))
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.Environment qualified as Env
import Data.HashMap.Strict.ExtendedSpec qualified as HashMapExtendedSpec
import Data.NonNegativeIntSpec qualified as NonNegetiveIntSpec
import Data.Parser.CacheControlSpec qualified as CacheControlParser
import Data.Parser.JSONPathSpec qualified as JsonPath
import Data.Time.Clock (getCurrentTime)
import Data.TimeSpec qualified as TimeSpec
import Data.URL.Template
import Database.MSSQL.TransactionSpec qualified as TransactionSpec
import Database.PG.Query qualified as Q
import Hasura.App
  ( PGMetadataStorageAppT (..),
    mkMSSQLSourceResolver,
    mkPgSourceResolver,
  )
import Hasura.Backends.MySQL.DataLoader.ExecuteTests qualified as MySQLDataLoader
import Hasura.EventingSpec qualified as EventingSpec
import Hasura.GraphQL.NamespaceSpec qualified as NamespaceSpec
import Hasura.GraphQL.Parser.DirectivesTest qualified as GraphQLDirectivesSpec
import Hasura.GraphQL.Schema.RemoteTest qualified as GraphRemoteSchemaSpec
import Hasura.IncrementalSpec qualified as IncrementalSpec
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.MetadataSpec qualified as MetadataSpec
import Hasura.RQL.PermissionSpec qualified as PermSpec
import Hasura.RQL.Types
import Hasura.RQL.Types.CommonSpec qualified as CommonTypesSpec
import Hasura.RQL.Types.EndpointSpec qualified as EndpointSpec
import Hasura.RQL.WebhookTransformsSpec qualified as WebhookTransformsSpec
import Hasura.SQL.WKTSpec qualified as WKTSpec
import Hasura.Server.Auth.JWTSpec qualified as JWTSpec
import Hasura.Server.AuthSpec qualified as AuthSpec
import Hasura.Server.Init
import Hasura.Server.Migrate
import Hasura.Server.MigrateSpec qualified as MigrateSpec
import Hasura.Server.TelemetrySpec qualified as TelemetrySpec
import Hasura.Server.Types
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Client.TransformableSpec qualified as TransformableSpec
import Options.Applicative
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import Test.Hspec
import Test.Hspec.Runner qualified as Hspec

data TestSuites
  = -- | Run all test suites. It probably doesn't make sense to be able to specify additional
    -- hspec args here.
    AllSuites
  | -- | Args to pass through to hspec (as if from 'getArgs'), and the specific suite to run.
    SingleSuite ![String] !TestSuite

data TestSuite
  = UnitSuite
  | PostgresSuite
  | MSSQLSuite

main :: IO ()
main =
  parseArgs >>= \case
    AllSuites -> do
      postgresSpecs <- buildPostgresSpecs
      mssqlSpecs <- buildMSSQLSpecs
      runHspec [] (unitSpecs *> postgresSpecs *> mssqlSpecs)
    SingleSuite hspecArgs suite ->
      runHspec hspecArgs =<< case suite of
        UnitSuite -> pure unitSpecs
        PostgresSuite -> buildPostgresSpecs
        MSSQLSuite -> buildMSSQLSpecs

unitSpecs :: Spec
unitSpecs = do
  describe "Data.HashMap.Strict.ExtendedSpec" HashMapExtendedSpec.spec
  describe "Data.NonNegativeInt" NonNegetiveIntSpec.spec
  describe "Data.Parser.CacheControl" CacheControlParser.spec
  describe "Data.Parser.JSONPath" JsonPath.spec
  describe "Data.Time" TimeSpec.spec
  describe "Hasura.Backends.MySQL.DataLoader.ExecuteTests" MySQLDataLoader.spec
  describe "Hasura.Eventing" EventingSpec.spec
  describe "Hasura.GraphQL.Parser.Directives" GraphQLDirectivesSpec.spec
  describe "Hasura.GraphQL.Schema.Remote" GraphRemoteSchemaSpec.spec
  describe "Hasura.Incremental" IncrementalSpec.spec
  describe "Hasura.RQL.Types.Common" CommonTypesSpec.spec
  describe "Hasura.RQL.Types.Endpoint" EndpointSpec.spec
  describe "Hasura.GraphQL.Namespace" NamespaceSpec.spec
  describe "Hasura.SQL.WKT" WKTSpec.spec
  describe "Hasura.Server.Auth.JWT" JWTSpec.spec
  describe "Hasura.Server.Auth" AuthSpec.spec
  describe "Hasura.Server.Telemetry" TelemetrySpec.spec
  describe "Hasura.RQL.PermissionSpec" PermSpec.spec
  describe "Hasura.RQL.MetadataSpec" MetadataSpec.spec
  describe "Hasura.RQL.WebhookTransformsSpec" WebhookTransformsSpec.spec
  describe "Network.HTTP.Client.TransformableSpec" TransformableSpec.spec

buildMSSQLSpecs :: IO Spec
buildMSSQLSpecs = do
  env <- liftIO getEnvironment
  connStr <- flip onLeft printErrExit $
    runWithEnv env $ do
      let envVar = fst mssqlConnectionString
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $
          "Expected: " <> envVar
  pure $ describe "Database.MSSQL.TransactionSpec" $ TransactionSpec.spec connStr

mssqlConnectionString :: (String, String)
mssqlConnectionString =
  ( "HASURA_MSSQL_CONN_STR",
    "SQL Server database connection string. Example DRIVER={ODBC Driver 17 for SQL Server};SERVER=localhost,1433;Uid=user;Pwd=pass;"
  )

buildPostgresSpecs :: IO Spec
buildPostgresSpecs = do
  env <- getEnvironment
  let envMap = Env.mkEnvironment env

  pgUrlText <- flip onLeft printErrExit $
    runWithEnv env $ do
      let envVar = fst databaseUrlEnv
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $ "Expected: " <> envVar

  let pgConnInfo = Q.ConnInfo 1 $ Q.CDDatabaseURI $ txtToBs pgUrlText
      urlConf = UrlValue $ InputWebhook $ mkPlainURLTemplate pgUrlText
      sourceConnInfo =
        PostgresSourceConnInfo urlConf (Just setPostgresPoolSettings) True Q.ReadCommitted Nothing
      sourceConfig = PostgresConnConfiguration sourceConnInfo Nothing

  pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams {Q.cpConns = 1} print
  let pgContext = mkPGExecCtx Q.Serializable pgPool

      logger :: Logger Hasura = Logger $ \l -> do
        let (logLevel, logType :: EngineLogType Hasura, logDetail) = toEngineLog l
        t <- liftIO $ getFormattedTime Nothing
        liftIO $ putStrLn $ LBS.toString $ A.encode $ EngineLog t logLevel logType logDetail

      setupCacheRef = do
        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        let sqlGenCtx = SQLGenCtx False False False
            maintenanceMode = MaintenanceModeDisabled
            readOnlyMode = ReadOnlyModeDisabled
            serverConfigCtx =
              ServerConfigCtx FunctionPermissionsInferred RemoteSchemaPermsDisabled sqlGenCtx maintenanceMode mempty EventingEnabled readOnlyMode
            cacheBuildParams = CacheBuildParams httpManager (mkPgSourceResolver print) mkMSSQLSourceResolver serverConfigCtx
            pgLogger = print

            run :: MetadataStorageT (PGMetadataStorageAppT CacheBuild) a -> IO a
            run =
              runMetadataStorageT
                >>> flip runPGMetadataStorageAppT (pgPool, pgLogger)
                >>> runCacheBuild cacheBuildParams
                >>> runExceptT
                >=> flip onLeft printErrJExit
                >=> flip onLeft printErrJExit

        (metadata, schemaCache) <- run do
          metadata <-
            snd
              <$> (liftEitherM . runExceptT . runTx pgContext Q.ReadWrite)
                (migrateCatalog (Just sourceConfig) maintenanceMode =<< liftIO getCurrentTime)
          schemaCache <- lift $ lift $ buildRebuildableSchemaCache logger envMap metadata
          pure (metadata, schemaCache)

        cacheRef <- newMVar schemaCache
        pure $ NT (run . flip MigrateSpec.runCacheRefT cacheRef . fmap fst . runMetadataT metadata)

  pure $
    beforeAll setupCacheRef $
      describe "Hasura.Server.Migrate" $ MigrateSpec.spec sourceConfig pgContext pgConnInfo

parseArgs :: IO TestSuites
parseArgs =
  execParser $
    info (helper <*> (parseNoCommand <|> parseSubCommand)) $
      fullDesc <> header "Hasura GraphQL Engine test suite"
  where
    parseNoCommand = pure AllSuites
    parseSubCommand = SingleSuite <$> parseHspecPassThroughArgs <*> subCmd
      where
        subCmd =
          subparser $
            mconcat
              [ command "unit" $
                  info (pure UnitSuite) $
                    progDesc "Only run unit tests",
                command "postgres" $
                  info (pure PostgresSuite) $
                    progDesc "Only run Postgres integration tests",
                command "mssql" $
                  info (pure MSSQLSuite) $
                    progDesc "Only run SQL Server unit tests"
              ]
        -- Add additional arguments and tweak as needed:
        hspecArgs = ["match", "skip"]
        -- parse to a list of arguments as they'd appear from 'getArgs':
        parseHspecPassThroughArgs :: Parser [String]
        parseHspecPassThroughArgs = fmap concat $
          for hspecArgs $ \nm ->
            fmap (maybe [] (\a -> ["--" <> nm, a])) $
              optional $
                strOption
                  ( long nm
                      <> metavar "<PATTERN>"
                      <> help "Flag passed through to hspec (see hspec docs)."
                  )

runHspec :: [String] -> Spec -> IO ()
runHspec hspecArgs m = do
  config <- Hspec.readConfig Hspec.defaultConfig hspecArgs
  Hspec.evaluateSummary =<< Hspec.runSpec m config

printErrExit :: String -> IO a
printErrExit = (*> exitFailure) . putStrLn

printErrJExit :: (A.ToJSON a) => a -> IO b
printErrJExit = (*> exitFailure) . BL.putStrLn . A.encode
