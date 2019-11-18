module Main (main) where

import           Hasura.Prelude

import           Options.Applicative
import           System.Environment        (getEnvironment)
import           System.Exit               (exitFailure)
import           Test.Hspec

import qualified Database.PG.Query         as Q
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP
import qualified Test.Hspec.Runner         as Hspec

import           Hasura.Db                 (PGExecCtx (..))
import           Hasura.RQL.Types          (SQLGenCtx (..), adminUserInfo)
import           Hasura.Server.Init        (RawConnInfo, mkConnInfo,
                                            mkRawConnInfo, parseRawConnInfo,
                                            runWithEnv)
import           Hasura.Server.Query       (RunCtx (..))

import qualified Hasura.IncrementalSpec    as IncrementalSpec
import qualified Hasura.RQL.MetadataSpec   as MetadataSpec
import qualified Hasura.Server.MigrateSpec as MigrateSpec

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
    UnitSuite -> runHspec unitSpecs
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
  pgPool <- Q.initPGPool pgConnInfo Q.defaultConnParams { Q.cpConns = 1 } print

  httpManager <- HTTP.newManager HTTP.tlsManagerSettings
  let runContext = RunCtx adminUserInfo httpManager (SQLGenCtx False)
      pgContext = PGExecCtx pgPool Q.Serializable

  pure $ describe "Hasura.Server.Migrate" $ MigrateSpec.spec pgConnInfo runContext pgContext

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
