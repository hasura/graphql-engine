{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Concurrent.MVar
import Control.Natural ((:~>) (..))
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.Environment qualified as Env
import Data.Time.Clock (getCurrentTime)
import Data.URL.Template
import Database.MSSQL.TransactionSuite qualified as TransactionSuite
import Database.PG.Query qualified as PG
import Discover qualified
import Hasura.App
  ( PGMetadataStorageAppT (..),
    mkMSSQLSourceResolver,
    mkPgSourceResolver,
  )
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.Connection.Settings
import Hasura.Backends.Postgres.Execute.Types
import Hasura.EventTriggerCleanupSuite qualified as EventTriggerCleanupSuite
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Schema.Cache
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.Server.Init
import Hasura.Server.Migrate
import Hasura.Server.MigrateSuite qualified as MigrateSuite
import Hasura.Server.Types
import Hasura.StreamingSubscriptionSuite qualified as StreamingSubscriptionSuite
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
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
main = do
  parseArgs >>= \case
    AllSuites -> do
      streamingSubscriptionSuite <- StreamingSubscriptionSuite.buildStreamingSubscriptionSuite
      eventTriggerLogCleanupSuite <- EventTriggerCleanupSuite.buildEventTriggerCleanupSuite
      postgresSpecs <- buildPostgresSpecs
      mssqlSpecs <- buildMSSQLSpecs
      runHspec [] (Discover.spec *> postgresSpecs *> mssqlSpecs *> streamingSubscriptionSuite *> eventTriggerLogCleanupSuite)
    SingleSuite hspecArgs suite -> do
      runHspec hspecArgs =<< case suite of
        UnitSuite -> pure Discover.spec
        PostgresSuite -> buildPostgresSpecs
        MSSQLSuite -> buildMSSQLSpecs

buildMSSQLSpecs :: IO (SpecWith ())
buildMSSQLSpecs = do
  env <- liftIO getEnvironment
  connStr <- flip onLeft printErrExit $
    runWithEnv env $ do
      let envVar = fst mssqlConnectionString
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $
          "Expected: " <> envVar

  -- We use "suite" to denote a set of tests that can't (yet) be detected and
  -- run by @hspec-discover@.
  pure $ describe "Database.MSSQL.TransactionSuite" $ TransactionSuite.suite connStr

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
      let envVar = _envVar databaseUrlOption
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $ "Expected: " <> envVar

  let pgConnInfo = PG.ConnInfo 1 $ PG.CDDatabaseURI $ txtToBs pgUrlText
      urlConf = UrlValue $ InputWebhook $ mkPlainURLTemplate pgUrlText
      sourceConnInfo =
        PostgresSourceConnInfo urlConf (Just setPostgresPoolSettings) True PG.ReadCommitted Nothing
      sourceConfig = PostgresConnConfiguration sourceConnInfo Nothing defaultPostgresExtensionsSchema

  pgPool <- PG.initPGPool pgConnInfo PG.defaultConnParams {PG.cpConns = 1} print
  let pgContext = mkPGExecCtx PG.Serializable pgPool NeverResizePool

      logger :: Logger Hasura = Logger $ \l -> do
        let (logLevel, logType :: EngineLogType Hasura, logDetail) = toEngineLog l
        t <- liftIO $ getFormattedTime Nothing
        liftIO $ putStrLn $ LBS.toString $ A.encode $ EngineLog t logLevel logType logDetail

      setupCacheRef = do
        httpManager <- HTTP.newManager HTTP.tlsManagerSettings
        let sqlGenCtx =
              SQLGenCtx
                Options.Don'tStringifyNumbers
                Options.Don'tDangerouslyCollapseBooleans
                Options.Don'tOptimizePermissionFilters
                Options.EnableBigQueryStringNumericInput
            maintenanceMode = MaintenanceModeDisabled
            readOnlyMode = ReadOnlyModeDisabled
            serverConfigCtx =
              ServerConfigCtx
                Options.InferFunctionPermissions
                Options.DisableRemoteSchemaPermissions
                sqlGenCtx
                maintenanceMode
                mempty
                EventingEnabled
                readOnlyMode
                Nothing -- We are not testing the naming convention here, so defaulting to hasura-default
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
              <$> (liftEitherM . runExceptT . runTx pgContext PG.ReadWrite)
                (migrateCatalog (Just sourceConfig) defaultPostgresExtensionsSchema maintenanceMode =<< liftIO getCurrentTime)
          schemaCache <- lift $ lift $ buildRebuildableSchemaCache logger envMap metadata
          pure (metadata, schemaCache)

        cacheRef <- newMVar schemaCache
        pure $ NT (run . flip MigrateSuite.runCacheRefT cacheRef . fmap fst . runMetadataT metadata)

  -- We use "suite" to denote a set of tests that can't (yet) be detected and
  -- run by @hspec-discover@.
  streamingSubscriptionSuite <- StreamingSubscriptionSuite.buildStreamingSubscriptionSuite
  eventTriggerLogCleanupSuite <- EventTriggerCleanupSuite.buildEventTriggerCleanupSuite

  pure $ do
    describe "Migrate suite" $
      beforeAll setupCacheRef $
        describe "Hasura.Server.Migrate" $ MigrateSuite.suite sourceConfig pgContext pgConnInfo
    describe "Streaming subscription suite" $ streamingSubscriptionSuite
    describe "Event trigger log cleanup suite" $ eventTriggerLogCleanupSuite

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
