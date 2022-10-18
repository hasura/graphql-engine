module Command
  ( Command (..),
    TestConfig (..),
    NameCasing (..),
    TestOptions (..),
    parseCommandLine,
  )
where

import Control.Arrow (left)
import Data.Aeson (FromJSON (..), eitherDecodeStrict')
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Version (showVersion)
import Hasura.Backends.DataConnector.API qualified as API
import Options.Applicative
import Paths_dc_api qualified as PackageInfo
import Servant.Client (BaseUrl, parseBaseUrl)
import Prelude

data Command
  = Test TestOptions
  | ExportOpenAPISpec

data TestConfig = TestConfig
  { _tcTableNamePrefix :: [Text],
    _tcTableNameCasing :: NameCasing,
    _tcColumnNameCasing :: NameCasing
  }

data NameCasing
  = PascalCase
  | Lowercase
  deriving (Eq, Show, Read)

data TestOptions = TestOptions
  { _toAgentBaseUrl :: BaseUrl,
    _toAgentConfig :: API.Config,
    _toTestConfig :: TestConfig,
    _toParallelDegree :: Maybe Int,
    _toMatch :: Maybe String,
    _toSkip :: [String],
    _toDryRun :: Bool,
    _toExportMatchStrings :: Bool
  }

parseCommandLine :: IO Command
parseCommandLine =
  execParser $
    info
      (helper <*> version <*> commandParser)
      ( fullDesc
          <> header "Hasura Data Connector Agent Test Utility"
      )

version :: Parser (a -> a)
version =
  infoOption
    displayText
    ( long "version"
        <> short 'v'
        <> help "Prints the version of the application and quits"
        <> hidden
    )
  where
    displayText = "Version " <> showVersion PackageInfo.version

commandParser :: Parser Command
commandParser =
  subparser
    (testCommand <> exportOpenApiSpecCommand)
  where
    testCommand =
      command
        "test"
        ( info
            (helper <*> testCommandParser)
            (progDesc "Executes a suite of tests against an agent to ensure its correct function")
        )
    exportOpenApiSpecCommand =
      command
        "export-openapi-spec"
        ( info
            (helper <*> pure ExportOpenAPISpec)
            (progDesc "Exports the OpenAPI specification of the Data Connector API that agents must implement")
        )

testConfigParser :: Parser TestConfig
testConfigParser =
  TestConfig
    <$> option
      jsonValue
      ( long "table-name-prefix"
          <> short 't'
          <> metavar "PREFIX"
          <> help "The prefix to use for all table names, as a JSON array of strings"
          <> value []
      )
    <*> option
      auto
      ( long "table-name-casing"
          <> metavar "CASING"
          <> help "The casing style to use for table names (PascalCase or Lowercase). Default: PascalCase"
          <> value PascalCase
      )
    <*> option
      auto
      ( long "column-name-casing"
          <> metavar "CASING"
          <> help "The casing style to use for column names (PascalCase or Lowercase). Default: PascalCase"
          <> value PascalCase
      )

testOptionsParser :: Parser TestOptions
testOptionsParser =
  TestOptions
    <$> option
      baseUrl
      ( long "agent-base-url"
          <> short 'u'
          <> metavar "URL"
          <> help "The base URL of the Data Connector agent to be tested"
      )
    <*> option
      configValue
      ( long "agent-config"
          <> short 's'
          <> metavar "JSON"
          <> help "The configuration JSON to be sent to the agent via the X-Hasura-DataConnector-Config header"
      )
    <*> testConfigParser
    <*> optional
      ( option
          positiveNonZeroInt
          ( long "jobs"
              <> short 'j'
              <> metavar "INT"
              <> help "Run at most N parallelizable tests simultaneously (default: number of available processors)"
          )
      )
    <*> optional
      ( strOption
          ( long "match"
              <> short 'm'
              <> metavar "PATTERN"
              <> help "Only run tests that match given PATTERN"
          )
      )
    <*> many
      ( strOption
          ( long "skip"
              <> short 's'
              <> metavar "PATTERN"
              <> help "Skip tests that match given PATTERN"
          )
      )
    <*> switch
      ( long "dry-run"
          <> help "Skip execution of test bodies"
      )
    <*> switch
      ( long "export-match-strings"
          <> help "Exports the hspec match strings without running the tests"
      )

testCommandParser :: Parser Command
testCommandParser = Test <$> testOptionsParser

baseUrl :: ReadM BaseUrl
baseUrl = eitherReader $ left show . parseBaseUrl

positiveNonZeroInt :: ReadM Int
positiveNonZeroInt =
  auto >>= \int ->
    if int <= 0 then readerError "Must be a positive, non-zero integer" else pure int

configValue :: ReadM API.Config
configValue = fmap API.Config jsonValue

jsonValue :: FromJSON v => ReadM v
jsonValue = eitherReader (eitherDecodeStrict' . Text.encodeUtf8 . Text.pack)
