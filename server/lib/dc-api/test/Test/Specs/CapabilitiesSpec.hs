module Test.Specs.CapabilitiesSpec (spec) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (forM_)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Hasura.Backends.DataConnector.API
import Language.GraphQL.Draft.Syntax qualified as G
import Test.AgentAPI (getSourceNameAndConfig)
import Test.AgentClient (AgentClientT, HasAgentClient)
import Test.AgentDatasets (HasDatasetContext)
import Test.AgentTestContext (HasAgentTestContext)
import Test.Expectations (jsonShouldBe)
import Test.Sandwich (ExampleT, HasBaseContext, describe, shouldNotContain)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

spec :: CapabilitiesResponse -> AgentDatasetTestSpec
spec CapabilitiesResponse {_crCapabilities = Capabilities {..}, ..} = describe "capabilities API" $ do
  it "returns a schema that can be used to validate the current config" $ do
    (_sourceName, config) <- getSourceNameAndConfig
    validateConfigAgainstConfigSchema _crConfigSchemaResponse config `jsonShouldBe` []

  describe "Scalar Types" $ do
    testPerScalarType "does not use any reserved comparison operator names" $ \_scalarType ScalarTypeCapabilities {..} -> do
      let names = fmap G.unName . HashMap.keys $ unComparisonOperators _stcComparisonOperators
      -- The built in types will generate graphql fields with these names
      let builtInGraphqlFieldNames = ["_eq", "_neq", "_in", "_nin", "_gt", "_lt", "_gte", "_lte", "_is_null"]
      -- The built in types have these names, so any custom operator that uses these names will overlap with them in the API JSON
      let builtInTypeNames = ["less_than", "less_than_or_equal", "greater_than", "greater_than_or_equal", "equal", "in", "is_null"]
      names `shouldNotContain` (builtInGraphqlFieldNames ++ builtInTypeNames)

    testPerScalarType "does not use any reserved update column operator names" $ \_scalarType ScalarTypeCapabilities {..} -> do
      let names = fmap (G.unName . unUpdateColumnOperatorName) . HashMap.keys $ unUpdateColumnOperators _stcUpdateColumnOperators
      names `shouldNotContain` ["set"]
  where
    testPerScalarType ::
      String ->
      ( forall context m.
        ( MonadThrow m,
          MonadIO m,
          HasBaseContext context,
          HasAgentClient context,
          HasAgentTestContext context,
          HasDatasetContext context
        ) =>
        ScalarType ->
        ScalarTypeCapabilities ->
        AgentClientT (ExampleT context m) ()
      ) ->
      AgentDatasetTestSpec
    testPerScalarType description test =
      describe description $ do
        forM_ (HashMap.toList $ unScalarTypesCapabilities _cScalarTypes) $ \(scalarType, scalarTypeCapabilities) -> do
          it (Text.unpack $ getScalarType scalarType) $
            test scalarType scalarTypeCapabilities
