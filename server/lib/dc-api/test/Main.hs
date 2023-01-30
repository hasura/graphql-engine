module Main (main) where

--------------------------------------------------------------------------------

import Command (AgentOptions (..), Command (..), SandwichArguments (..), TestOptions (..), parseCommandLine)
import Control.Exception (bracket)
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Text.Lazy.IO qualified as Text
import Hasura.Backends.DataConnector.API (openApiSchema)
import Hasura.Backends.DataConnector.API qualified as API
import Servant.Client ((//))
import System.Environment (withArgs)
import Test.AgentAPI (guardCapabilitiesResponse, guardSchemaResponse)
import Test.AgentClient (AgentIOClient (..), introduceAgentClient, mkAgentClientConfig, mkAgentIOClient)
import Test.AgentDatasets (DatasetCloneInfo (..), chinookTemplate, createClone, deleteClone, usesDataset)
import Test.AgentTestContext (AgentTestContext (..), introduceAgentTestContext)
import Test.Data (TestData, mkTestData)
import Test.DataExport (exportData)
import Test.Sandwich (runSandwichWithCommandLineArgs)
import Test.Sandwich.Options qualified as Sandwich
import Test.Specs.CapabilitiesSpec qualified
import Test.Specs.ErrorSpec qualified
import Test.Specs.ExplainSpec qualified
import Test.Specs.HealthSpec qualified
import Test.Specs.MetricsSpec qualified
import Test.Specs.QuerySpec qualified
import Test.Specs.SchemaSpec qualified
import Test.TestHelpers (AgentTestSpec)
import Prelude

--------------------------------------------------------------------------------

testSourceName :: API.SourceName
testSourceName = "dc-api-tests"

tests :: TestData -> API.CapabilitiesResponse -> AgentTestSpec
tests testData capabilitiesResponse@API.CapabilitiesResponse {..} = do
  usesDataset chinookTemplate $ do
    Test.Specs.HealthSpec.spec
    Test.Specs.CapabilitiesSpec.spec capabilitiesResponse
    Test.Specs.SchemaSpec.spec testData _crCapabilities
    Test.Specs.QuerySpec.spec testData _crCapabilities
    Test.Specs.ErrorSpec.spec testData
    for_ (API._cMetrics _crCapabilities) \m -> Test.Specs.MetricsSpec.spec m
    for_ (API._cExplain _crCapabilities) \_ -> Test.Specs.ExplainSpec.spec testData _crCapabilities

getChinookSchema :: API.Capabilities -> Maybe API.Config -> AgentIOClient -> IO API.SchemaResponse
getChinookSchema API.Capabilities {..} manuallyProvidedConfig (AgentIOClient agentClient) = do
  case manuallyProvidedConfig of
    Just config -> (agentClient // API._schema) testSourceName config >>= guardSchemaResponse
    Nothing ->
      if isJust _cDatasets
        then
          bracket
            (createClone agentClient chinookTemplate)
            (deleteClone agentClient)
            ( \DatasetCloneInfo {..} ->
                (agentClient // API._schema) testSourceName _dciAgentConfig >>= guardSchemaResponse
            )
        else fail $ "The agent does not support datasets, therefore an agent configuration must be provided on the command line (--agent-config)"

main :: IO ()
main = do
  command <- parseCommandLine
  case command of
    Test TestOptions {..} (SandwichArguments arguments) -> withArgs arguments $ do
      agentIOClient@(AgentIOClient agentClient) <- mkAgentIOClient _toSensitiveOutputHandling (_aoAgentBaseUrl _toAgentOptions)
      agentCapabilities <- (agentClient // API._capabilities) >>= guardCapabilitiesResponse
      chinookSchema <- getChinookSchema (API._crCapabilities agentCapabilities) (_aoAgentConfig _toAgentOptions) agentIOClient

      agentClientConfig <- mkAgentClientConfig _toSensitiveOutputHandling (_aoAgentBaseUrl _toAgentOptions)
      let testData = mkTestData chinookSchema _toTestConfig
      let testContext = AgentTestContext testSourceName agentCapabilities (_aoAgentConfig _toAgentOptions)
      runSandwichWithCommandLineArgs Sandwich.defaultOptions $
        introduceAgentTestContext testContext . introduceAgentClient agentClientConfig $
          tests testData agentCapabilities
      pure ()
    ExportOpenAPISpec ->
      Text.putStrLn $ encodeToLazyText openApiSchema
    ExportData config ->
      exportData config

  pure ()
