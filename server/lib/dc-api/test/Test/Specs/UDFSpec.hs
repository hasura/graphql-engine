{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module containing tests for user-defined-functions aka. "UDFs".
module Test.Specs.UDFSpec (spec) where

--------------------------------------------------------------------------------

import Command (TestConfig)
import Control.Lens ((?~))
import Control.Lens.Lens ((&))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value (..))
import Data.List (sort)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API
import Hasura.Backends.DataConnector.API qualified as API
import Test.AgentAPI (getSchemaGuarded, queryGuarded)
import Test.AgentClient (HasAgentClient, runAgentClientT)
import Test.AgentDatasets (HasDatasetContext)
import Test.AgentTestContext (HasAgentTestContext)
import Test.Data (FunctionsTestData (..), mkFunctionsTestData)
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe, rowsShouldBe)
import Test.Sandwich (HasLabel, Label (..), LabelValue, SpecFree, describe, getContext, introduce, (:>))
import Test.Sandwich.Misc (HasBaseContext)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

--------------------------------------------------------------------------------

fibonacciRows :: Int
fibonacciRows = 5 -- TODO: Make this a Gen

spec :: TestConfig -> API.Capabilities -> AgentDatasetTestSpec
spec testConfig API.Capabilities {} = describe "supports functions" $ preloadAgentSchema $ do
  -- TODO: Check that the expected test tables are present too. Will require test-data changes.
  it "returns functions from the Functions dataset" do
    preloadedSchema <- getPreloadedAgentSchema

    let FunctionsTestData {..} = mkFunctionsTestData preloadedSchema testConfig
        extractFunctionNames = sort . fmap API._fiName
        expectedFunctionNames = [_ftdFibonacciFunctionName, _ftdSearchArticlesFunctionName]

    functionNames <- (extractFunctionNames . API._srFunctions) <$> getPreloadedAgentSchema
    functionNames `jsonShouldBe` expectedFunctionNames

  it "can query for a list Fibonacci numbers using the fibonacci function" $ do
    preloadedSchema <- getPreloadedAgentSchema

    let testData@FunctionsTestData {..} = mkFunctionsTestData preloadedSchema testConfig
        query = fibonacciRequest testData

    results <- queryGuarded query

    Data.responseRows results `rowsShouldBe` _ftdFibonacciRows fibonacciRows
    _qrAggregates results `jsonShouldBe` Nothing
  where
    fibonacciRequest :: FunctionsTestData -> QueryRequest
    fibonacciRequest FunctionsTestData {..} =
      let fields = Data.mkFieldsMap [("Value", _ftdFunctionField _ftdFibonacciFunctionName "Value")]
          query = Data.emptyQuery & qFields ?~ fields
          k = "take" :: Text.Text
          v = API.ScalarValue (Number (fromIntegral fibonacciRows)) (API.ScalarType "number")
          args = [NamedArgument k (API.ScalarArgumentValue v)]
       in QRFunction $ FunctionRequest _ftdFibonacciFunctionName args mempty query

type AgentSchemaLabel = "agent-schema"

preloadAgentSchema :: forall context m. (MonadIO m, MonadThrow m, HasAgentClient context, HasBaseContext context, HasAgentTestContext context, HasDatasetContext context) => SpecFree (LabelValue AgentSchemaLabel API.SchemaResponse :> context) m () -> SpecFree context m ()
preloadAgentSchema = introduce "Preload agent schema" agentSchemaLabel getAgentSchema (const $ pure ())
  where
    getAgentSchema = runAgentClientT Nothing $ getSchemaGuarded

agentSchemaLabel :: Label AgentSchemaLabel API.SchemaResponse
agentSchemaLabel = Label

type HasPreloadedAgentSchema context = HasLabel context "agent-schema" API.SchemaResponse

getPreloadedAgentSchema :: (HasCallStack, HasPreloadedAgentSchema context, MonadReader context m) => m API.SchemaResponse
getPreloadedAgentSchema = getContext agentSchemaLabel
