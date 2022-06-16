{-# LANGUAGE TemplateHaskell #-}

module Command
  ( Command (..),
    TestOptions (..),
    AgentCapabilities (..),
    parseCommandLine,
  )
where

import Control.Arrow (left)
import Control.Lens (contains, modifying, use, (^.), _2)
import Control.Lens.TH (makeLenses)
import Control.Monad (when)
import Control.Monad.State (State, runState)
import Data.Aeson (eitherDecodeStrict')
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Version (showVersion)
import Hasura.Backends.DataConnector.API qualified as API
import Options.Applicative
import Paths_graphql_engine qualified as PackageInfo
import Servant.Client (BaseUrl, parseBaseUrl)
import Prelude

data Command
  = Test TestOptions
  | ExportOpenAPISpec

data TestOptions = TestOptions
  { _toAgentBaseUrl :: BaseUrl,
    _toAgentConfig :: API.Config,
    _toAgentCapabilities :: AgentCapabilities,
    _toParallelDegree :: Maybe Int,
    _toMatch :: Maybe String,
    _toSkip :: Maybe String
  }

data AgentCapabilities
  = AutoDetect
  | Explicit API.Capabilities

data CapabilitiesState = CapabilitiesState
  { _csRemainingCapabilities :: HashSet Text,
    _csCapabilitiesEnquired :: HashSet Text
  }

$(makeLenses ''CapabilitiesState)

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
    <*> agentCapabilitiesParser
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
    <*> optional
      ( option
          auto
          ( long "skip"
              <> short 's'
              <> metavar "PATTERN"
              <> help "Skip tests that match given PATTERN"
          )
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
configValue = eitherReader $ (fmap API.Config . eitherDecodeStrict' . Text.encodeUtf8 . Text.pack)

agentCapabilitiesParser :: Parser AgentCapabilities
agentCapabilitiesParser =
  option
    agentCapabilities
    ( long "capabilities"
        <> short 'c'
        <> metavar "CAPABILITIES"
        <> value AutoDetect
        <> help (Text.unpack helpText)
    )
  where
    helpText =
      "The capabilities that the agent has, to determine what tests to run. By default, they will be autodetected. The valid capabilities are: " <> allCapabilitiesText
    allCapabilitiesText =
      "[autodetect | none | " <> Text.intercalate "," (HashSet.toList allPossibleCapabilities) <> "]"

agentCapabilities :: ReadM AgentCapabilities
agentCapabilities =
  str >>= \text -> do
    let capabilities = HashSet.fromList $ Text.strip <$> Text.split (== ',') text
    if HashSet.member "autodetect" capabilities
      then
        if HashSet.size capabilities == 1
          then pure AutoDetect
          else readerError "You can either autodetect capabilities or specify them manually, not both"
      else
        if HashSet.member "none" capabilities
          then
            if HashSet.size capabilities == 1
              then pure . Explicit . fst $ readCapabilities mempty
              else readerError "You cannot specify other capabilities when specifying none"
          else Explicit <$> readExplicitCapabilities capabilities
  where
    readExplicitCapabilities :: HashSet Text -> ReadM API.Capabilities
    readExplicitCapabilities providedCapabilities =
      let (capabilities, CapabilitiesState {..}) = readCapabilities providedCapabilities
       in if _csRemainingCapabilities /= mempty
            then readerError . Text.unpack $ "Unknown capabilities: " <> Text.intercalate "," (HashSet.toList _csRemainingCapabilities)
            else pure capabilities

readCapabilities :: HashSet Text -> (API.Capabilities, CapabilitiesState)
readCapabilities providedCapabilities =
  flip runState (CapabilitiesState providedCapabilities mempty) $ do
    supportsRelationships <- readCapability "relationships"
    pure $ API.emptyCapabilities {API.cRelationships = if supportsRelationships then Just API.RelationshipCapabilities {} else Nothing}

readCapability :: Text -> State CapabilitiesState Bool
readCapability capability = do
  modifying csCapabilitiesEnquired $ HashSet.insert capability
  hasCapability <- use $ csRemainingCapabilities . contains capability
  when hasCapability $
    modifying csRemainingCapabilities $ HashSet.delete capability
  pure hasCapability

allPossibleCapabilities :: HashSet Text
allPossibleCapabilities =
  readCapabilities mempty ^. _2 . csCapabilitiesEnquired
