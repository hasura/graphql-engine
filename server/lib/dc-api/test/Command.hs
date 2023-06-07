{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use commaSeparated" #-}
{-# HLINT ignore "Use tshow" #-}

module Command
  ( Command (..),
    TestOptions (..),
    SensitiveOutputHandling (..),
    SandwichArguments (..),
    TestConfig (..),
    AgentOptions (..),
    AgentConfig (..),
    NameCasing (..),
    ExportDataConfig (..),
    ExportFormat (..),
    parseCommandLine,
  )
where

import Control.Arrow (left)
import Data.Aeson (FromJSON (..), eitherDecodeStrict')
import Data.List (intercalate)
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
  = Test TestOptions SandwichArguments
  | ExportOpenAPISpec
  | ExportData ExportDataConfig

data TestOptions = TestOptions
  { _toAgentOptions :: AgentOptions,
    _toTestConfig :: TestConfig,
    _toSensitiveOutputHandling :: SensitiveOutputHandling
  }

newtype SandwichArguments = SandwichArguments [String]

data TestConfig = TestConfig
  { _tcTableNamePrefix :: [Text],
    _tcTableNameCasing :: NameCasing,
    _tcFunctionNamePrefix :: [Text],
    _tcFunctionNameCasing :: NameCasing,
    _tcColumnNameCasing :: NameCasing
  }

data SensitiveOutputHandling
  = AllowSensitiveOutput
  | DisallowSensitiveOutput

data AgentOptions = AgentOptions
  { _aoAgentBaseUrl :: BaseUrl,
    _aoAgentConfig :: AgentConfig
  }

data AgentConfig
  = ManualConfig API.Config
  | DatasetConfig (Maybe API.Config)

data NameCasing
  = PascalCase
  | Lowercase
  | Uppercase
  deriving stock (Eq, Show, Read, Enum, Bounded)

data ExportDataConfig = ExportDataConfig
  { _edcDirectory :: FilePath,
    _edcFormat :: ExportFormat,
    _edcDateTimeFormat :: Maybe String
  }

data ExportFormat
  = JSON
  | JSONLines
  | SingleJSONFile
  deriving stock (Eq, Show, Read)

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
    (testCommand <> exportOpenApiSpecCommand <> exportData)
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
    exportData =
      command
        "export-data"
        ( info
            (helper <*> (ExportData <$> exportDataConfigParser))
            (progDesc "Exports the Chinook dataset to files in the specified directory")
        )

testCommandParser :: Parser Command
testCommandParser = Test <$> testOptionsParser <*> sandwichArgumentsParser

testOptionsParser :: Parser TestOptions
testOptionsParser =
  TestOptions
    <$> agentOptionsParser
    <*> testConfigParser
    <*> flag
      DisallowSensitiveOutput
      AllowSensitiveOutput
      ( long "allow-sensitive-output"
          <> help "Allows sensitive values (such as the X-Hasura-DataConnector-Config header) to appear in test debug output"
      )

sandwichArgumentsParser :: Parser SandwichArguments
sandwichArgumentsParser =
  subparser (command "sandwich" (info (SandwichArguments <$> many (strArgument mempty)) forwardOptions))
    <|> pure (SandwichArguments [])

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
          <> help ("The casing style to use for table names (" <> casingOptions <> "). Default: PascalCase")
          <> value PascalCase
      )
    <*> option
      jsonValue
      ( long "function-name-prefix"
          <> short 'f'
          <> metavar "FUNCTION_PREFIX"
          <> help "The prefix to use for all function names, as a JSON array of strings"
          <> value []
      )
    <*> option
      auto
      ( long "function-name-casing"
          <> metavar "FUNCTION_CASING"
          <> help ("The casing style to use for function names (" <> casingOptions <> "). Default: PascalCase")
          <> value PascalCase
      )
    <*> option
      auto
      ( long "column-name-casing"
          <> metavar "CASING"
          <> help ("The casing style to use for column names (" <> casingOptions <> "). Default: PascalCase")
          <> value PascalCase
      )
  where
    casingOptions = intercalate ", " $ show <$> enumFromTo @NameCasing minBound maxBound

agentOptionsParser :: Parser AgentOptions
agentOptionsParser =
  AgentOptions
    <$> option
      baseUrl
      ( long "agent-base-url"
          <> short 'u'
          <> metavar "URL"
          <> help "The base URL of the Data Connector agent to be tested"
      )
    <*> ( ManualConfig
            <$> option
              configValue
              ( long "agent-config"
                  <> metavar "JSON"
                  <> help "The configuration JSON to be sent to the agent via the X-Hasura-DataConnector-Config header. If omitted, datasets will be used to load test data and provide this configuration dynamically"
              )
              <|> DatasetConfig
            <$> optional
              ( option
                  configValue
                  ( long "merge-agent-config"
                      <> metavar "JSON"
                      <> help "Datasets will be used to load test data and provide configuration JSON to be sent to the agent via the X-Hasura-DataConnector-Config header. This config will be merged with the dataset-provided config before being sent to the agent."
                  )
              )
        )

exportDataConfigParser :: Parser ExportDataConfig
exportDataConfigParser =
  ExportDataConfig
    <$> strOption
      ( long "directory"
          <> short 'd'
          <> metavar "DIR"
          <> help "The directory to export the data files into"
      )
    <*> option
      auto
      ( long "format"
          <> short 'f'
          <> metavar "FORMAT"
          <> help "The format to export (JSON or JSONLines)"
      )
    <*> optional
      ( strOption
          ( long "datetime-format"
              <> metavar "FORMAT"
              <> help "Format string to use when formatting DateTime columns (use format syntax from https://hackage.haskell.org/package/time-1.12.2/docs/Data-Time-Format.html#v:formatTime)"
          )
      )

baseUrl :: ReadM BaseUrl
baseUrl = eitherReader $ left show . parseBaseUrl

configValue :: ReadM API.Config
configValue = fmap API.Config jsonValue

jsonValue :: (FromJSON v) => ReadM v
jsonValue = eitherReader (eitherDecodeStrict' . Text.encodeUtf8 . Text.pack)
