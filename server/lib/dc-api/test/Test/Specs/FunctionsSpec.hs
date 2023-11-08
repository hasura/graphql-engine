{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module containing tests for user-defined-functions
module Test.Specs.FunctionsSpec (spec) where

--------------------------------------------------------------------------------

import Command (TestConfig)
import Control.Lens ((<&>), (?~))
import Control.Lens.Lens ((&))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value (..))
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort, sortOn)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API
import Hasura.Backends.DataConnector.API qualified as API
import Test.AgentAPI (getSchemaGuarded, getSchemaGuarded', queryGuarded)
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

  it "returns the specified functions from the Functions dataset when filtered" do
    preloadedSchema <- getPreloadedAgentSchema
    let FunctionsTestData {..} = mkFunctionsTestData preloadedSchema testConfig
        extractFunctionNames = sort . fmap API._fiName
        desiredFunctions = [_ftdFibonacciFunctionName]
        filters = mempty {API._sfOnlyFunctions = Just desiredFunctions}

    functionNames <- extractFunctionNames . API._srFunctions <$> getSchemaGuarded' (API.SchemaRequest filters API.BasicInfo)
    functionNames `jsonShouldBe` desiredFunctions

  it "returns the no functions when filtered with an empty list" do
    let filters = mempty {API._sfOnlyFunctions = Just []}

    functionInfos <- API._srFunctions <$> getSchemaGuarded' (API.SchemaRequest filters API.BasicInfo)
    functionInfos `jsonShouldBe` []

  it "returns only Function names and types when using basic_info detail level" $ do
    preloadedSchema <- getPreloadedAgentSchema

    let FunctionsTestData {..} = mkFunctionsTestData preloadedSchema testConfig
        expectedFunctionNames = [_ftdFibonacciFunctionName, _ftdSearchArticlesFunctionName]

    functionInfos <- sortOn API._fiName . API._srFunctions <$> getSchemaGuarded' (API.SchemaRequest mempty API.BasicInfo)

    let expectedFunctionInfos =
          expectedFunctionNames
            <&> (\functionName -> API.FunctionInfo functionName API.FRead Nothing Nothing [] Nothing)

    functionInfos `jsonShouldBe` expectedFunctionInfos

  it "can query for a list Fibonacci numbers using the fibonacci function" $ do
    preloadedSchema <- getPreloadedAgentSchema

    let fibonacciRequest :: FunctionsTestData -> QueryRequest
        fibonacciRequest FunctionsTestData {..} =
          let fields = Data.mkFieldsMap [("Value", _ftdFibonacciField _ftdFibonacciFunctionName "Value")]
              query' = Data.emptyQuery & qFields ?~ fields
              k = "take" :: Text.Text
              v = API.ScalarValue (Number (fromIntegral fibonacciRows)) (API.ScalarType "number")
              args = [NamedArgument k (API.ScalarArgumentValue v)]
           in FunctionQueryRequest _ftdFibonacciFunctionName args mempty mempty mempty query' mempty

        testData@FunctionsTestData {..} = mkFunctionsTestData preloadedSchema testConfig
        query = fibonacciRequest testData

    results <- queryGuarded query

    Data.responseRows results `rowsShouldBe` _ftdFibonacciRows fibonacciRows
    _qrAggregates results `jsonShouldBe` Nothing

  -- Note: Reflection on return type should be done as part of the integration tests, not the agent tests.

  it "can traverse relationships from returned values" do
    preloadedSchema <- getPreloadedAgentSchema

    let articlesRequest :: FunctionsTestData -> QueryRequest
        articlesRequest FunctionsTestData {..} =
          let authorSubqueryFields = Data.mkFieldsMap [("Name", _ftdColumnField _ftdAuthorsTableName "name")]
              authorSubquery = Data.emptyQuery & qFields ?~ authorSubqueryFields
              fields =
                Data.mkFieldsMap
                  [ ("Title", _ftdSearchArticlesField _ftdSearchArticlesFunctionName "title"),
                    ("Author", RelField $ RelationshipField _ftdAuthorRelationshipName authorSubquery)
                  ]
              query' = Data.emptyQuery & qFields ?~ fields
              authorRelationship =
                let authorsJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "author_id", API.mkColumnSelector $ API.ColumnName "id")]
                 in API.FunctionRelationships
                      _ftdSearchArticlesFunctionName
                      ( HashMap.fromList
                          [ (_ftdAuthorRelationshipName, API.Relationship (API.TTargetTable _ftdAuthorsTableName) API.ObjectRelationship authorsJoinFieldMapping)
                          ]
                      )
              relationships = Set.singleton (API.RFunction authorRelationship)
              v = API.ScalarValue (String "x") (API.ScalarType "string")
              args = [NamedArgument "query" (API.ScalarArgumentValue v)]
           in FunctionQueryRequest _ftdSearchArticlesFunctionName args relationships mempty mempty query' mempty

        testData = mkFunctionsTestData preloadedSchema testConfig
        query = articlesRequest testData

    results <- queryGuarded query

    let expectedResponseRows =
          pure $
            Data.mkFieldsMap
              [ ("Title", API.mkColumnFieldValue (String "Don Quixote")),
                ( "Author",
                  API.mkNestedObjFieldValue $
                    HashMap.singleton (API.FieldName "rows") $
                      API.mkNestedArrayFieldValue $
                        pure $
                          API.mkNestedObjFieldValue $
                            HashMap.singleton
                              (API.FieldName "Name")
                              (API.mkColumnFieldValue (String "Darwin"))
                )
              ]

    Data.responseRows results `rowsShouldBe` expectedResponseRows

  it "correctly filters function-returned rows with a where predicate and limit" do
    preloadedSchema <- getPreloadedAgentSchema

    let articlesRequest :: FunctionsTestData -> QueryRequest
        articlesRequest FunctionsTestData {..} =
          let fields = Data.mkFieldsMap [("Title", _ftdSearchArticlesField _ftdSearchArticlesFunctionName "title")]
              whereClause =
                API.ApplyBinaryComparisonOperator
                  API.LessThan
                  (API.ComparisonColumn API.CurrentTable (API.mkColumnSelector $ API.ColumnName "id") (API.ScalarType "number") Nothing)
                  (API.ScalarValueComparison (API.ScalarValue (Number 10) (API.ScalarType "number")))
              query' = Data.emptyQuery & qFields ?~ fields & qWhere ?~ whereClause & qLimit ?~ 2
              authorRelationship =
                let authorsJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "author_id", API.mkColumnSelector $ API.ColumnName "id")]
                 in API.FunctionRelationships
                      _ftdSearchArticlesFunctionName
                      ( HashMap.fromList
                          [ (_ftdAuthorRelationshipName, API.Relationship (API.TTargetTable _ftdAuthorsTableName) API.ObjectRelationship authorsJoinFieldMapping)
                          ]
                      )
              relationships = Set.singleton (API.RFunction authorRelationship)
              v = API.ScalarValue (String "y") (API.ScalarType "string")
              args = [NamedArgument "query" (API.ScalarArgumentValue v)]
           in FunctionQueryRequest _ftdSearchArticlesFunctionName args relationships mempty mempty query' mempty

        testData = mkFunctionsTestData preloadedSchema testConfig
        query = articlesRequest testData

    results <- queryGuarded query

    let mkRow x = HashMap.singleton (API.FieldName "Title") (API.mkColumnFieldValue (String x))
        titles =
          [ "A Brief History of Time",
            "Relativity: The Special and General Theory"
          ]
        expectedResponseRows = map mkRow titles

    Data.responseRows results `rowsShouldBe` expectedResponseRows

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
