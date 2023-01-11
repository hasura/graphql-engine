module Main (main) where

--------------------------------------------------------------------------------

import Command (AgentOptions (..), Command (..), SandwichArguments (..), TestOptions (..), parseCommandLine)
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (for_)
import Data.Text.Lazy.IO qualified as Text
import Hasura.Backends.DataConnector.API (openApiSchema)
import Hasura.Backends.DataConnector.API qualified as API
import Servant.Client ((//))
import System.Environment (withArgs)
import Test.AgentClient (AgentIOClient (..), guardCapabilitiesResponse, guardSchemaResponse, introduceAgentClient, mkAgentClientConfig, mkAgentIOClient)
import Test.Data (TestData, mkTestData)
import Test.DataExport (exportData)
import Test.Sandwich (runSandwichWithCommandLineArgs)
import Test.Sandwich.Options qualified as Sandwich
import Test.Specs.CapabilitiesSpec qualified
import Test.Specs.ConfigSpec qualified
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

tests :: TestData -> API.SourceName -> API.Config -> API.CapabilitiesResponse -> AgentTestSpec
tests testData sourceName agentConfig API.CapabilitiesResponse {..} = do
  Test.Specs.HealthSpec.spec sourceName agentConfig
  Test.Specs.ConfigSpec.spec agentConfig _crConfigSchemaResponse
  Test.Specs.CapabilitiesSpec.spec agentConfig _crCapabilities
  Test.Specs.SchemaSpec.spec testData sourceName agentConfig _crCapabilities
  Test.Specs.QuerySpec.spec testData sourceName agentConfig _crCapabilities
  Test.Specs.ErrorSpec.spec testData sourceName agentConfig _crCapabilities
  for_ (API._cMetrics _crCapabilities) \m -> Test.Specs.MetricsSpec.spec m
  for_ (API._cExplain _crCapabilities) \_ -> Test.Specs.ExplainSpec.spec testData sourceName agentConfig _crCapabilities

main :: IO ()
main = do
  command <- parseCommandLine
  case command of
    Test TestOptions {..} (SandwichArguments arguments) -> withArgs arguments $ do
      (AgentIOClient agentClient) <- mkAgentIOClient _toSensitiveOutputHandling _toAgentOptions
      agentCapabilities <- (agentClient // API._capabilities) >>= guardCapabilitiesResponse
      schemaResponse <- (agentClient // API._schema) testSourceName (_aoAgentConfig _toAgentOptions) >>= guardSchemaResponse

      agentClientConfig <- mkAgentClientConfig _toSensitiveOutputHandling _toAgentOptions
      let testData = mkTestData schemaResponse _toTestConfig
      runSandwichWithCommandLineArgs Sandwich.defaultOptions $
        introduceAgentClient agentClientConfig $
          tests testData testSourceName (_aoAgentConfig _toAgentOptions) agentCapabilities
      pure ()
    ExportOpenAPISpec ->
      Text.putStrLn $ encodeToLazyText openApiSchema
    ExportData config ->
      exportData config

  pure ()
