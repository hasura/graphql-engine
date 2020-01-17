module Main (main) where

import           Hasura.Prelude

import           Control.Concurrent.MVar
import           Control.Natural              ((:~>) (..))
import           Data.Time.Clock              (getCurrentTime)
import           Options.Applicative
import           System.Environment           (getEnvironment)
import           System.Exit                  (exitFailure)
import           Test.Hspec

import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy.Char8   as BL
import qualified Database.PG.Query            as Q
import qualified Network.HTTP.Client          as HTTP
import qualified Network.HTTP.Client.TLS      as HTTP
import qualified Test.Hspec.Runner            as Hspec

import           Hasura.Db                    (PGExecCtx (..))
import           Hasura.RQL.Types             (SQLGenCtx (..), adminUserInfo)
import           Hasura.RQL.Types.Run
import           Hasura.Server.Init           (RawConnInfo, mkConnInfo, mkRawConnInfo,
                                               parseRawConnInfo, runWithEnv)
import           Hasura.Server.Migrate

import qualified Data.Parser.CacheControlSpec as CacheControlParser
import qualified Hasura.IncrementalSpec       as IncrementalSpec
import qualified Hasura.RQL.MetadataSpec      as MetadataSpec
import qualified Hasura.Server.MigrateSpec    as MigrateSpec

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
  describe "Data.Parser.CacheControl" CacheControlParser.spec
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
