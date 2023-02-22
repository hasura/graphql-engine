module Test.Specs.SchemaSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((%~), (.~), (?~))
import Control.Lens.At (at)
import Control.Lens.Lens ((&))
import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Object)
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Hasura.Backends.DataConnector.API qualified as API
import Test.AgentAPI (getSchemaGuarded)
import Test.AgentClient (AgentClientT, HasAgentClient)
import Test.AgentDatasets (HasDatasetContext)
import Test.AgentTestContext (HasAgentTestContext)
import Test.Data (TestData (..))
import Test.Expectations (jsonShouldBe)
import Test.Sandwich (ExampleT, describe)
import Test.Sandwich.Misc (HasBaseContext)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

--------------------------------------------------------------------------------

spec :: TestData -> API.Capabilities -> AgentDatasetTestSpec
spec TestData {..} API.Capabilities {..} = describe "schema API" $ do
  let supportsInserts = isJust $ _cMutations >>= API._mcInsertCapabilities
  let supportsUpdates = isJust $ _cMutations >>= API._mcUpdateCapabilities
  let supportsDeletes = isJust $ _cMutations >>= API._mcDeleteCapabilities

  it "returns the Chinook tables" $ do
    let extractTableNames = sort . fmap API._tiName
    tableNames <- (extractTableNames . API._srTables) <$> getSchemaGuarded

    let expectedTableNames = extractTableNames _tdSchemaTables
    tableNames `jsonShouldBe` expectedTableNames

  testPerTable "returns the correct columns in the Chinook tables" $ \expectedTable@API.TableInfo {..} -> do
    actualTable <- find (\t -> API._tiName t == _tiName) . API._srTables <$> getSchemaGuarded

    -- We remove some properties here so that we don't compare them since they vary between agent implementations
    let extractJsonForComparison table =
          let columns = fmap toJSON . sortOn API._ciName $ API._tiColumns table
           in columns
                & traverse %~ \column ->
                  column
                    & _Object . at "type" .~ Nothing -- Types can vary between agents since underlying datatypes can change
                    & _Object . at "description" .~ Nothing -- Descriptions are not supported by all agents
    let actualJsonColumns = extractJsonForComparison <$> actualTable
    let expectedJsonColumns =
          expectedTable
            & extractJsonForComparison
            -- If the agent only supports nullable columns, we make all columns nullable
            & applyWhen
              (API._dscColumnNullability _cDataSchema == API.OnlyNullableColumns)
              (traverse %~ (_Object . at "nullable" ?~ Bool True))
            -- If the agent doesn't support insert mutations then all columns should not be insertable
            & applyWhen
              (not supportsInserts)
              (traverse %~ (_Object . at "insertable" ?~ Bool False))
            -- If agent doesn't support update mutations then all columns should not be updatable
            & applyWhen
              (not supportsUpdates)
              (traverse %~ (_Object . at "updatable" ?~ Bool False))
            & Just

    actualJsonColumns `jsonShouldBe` expectedJsonColumns

  testPerTable "returns the correct mutability in the Chinook tables" $ \expectedTable@API.TableInfo {..} -> do
    actualTable <- find (\t -> API._tiName t == _tiName) . API._srTables <$> getSchemaGuarded

    let extractJsonForComparison (table :: API.TableInfo) =
          toJSON table
            & _Object %~ (KeyMap.filterWithKey (\prop _value -> prop `elem` ["insertable", "updatable", "deletable"]))

    let actualComparisonJson = extractJsonForComparison <$> actualTable
    let expectedComparisonJson =
          expectedTable
            & extractJsonForComparison
            -- If the agent doesn't support insert mutations then the table should not be insertable
            & applyWhen
              (not supportsInserts)
              (_Object . at "insertable" ?~ Bool False)
            -- If the agent doesn't support update mutations then the table should not be updatable
            & applyWhen
              (not supportsUpdates)
              (_Object . at "updatable" ?~ Bool False)
            -- If the agent doesn't support delete mutations then the table should not be deletable
            & applyWhen
              (not supportsDeletes)
              (_Object . at "deletable" ?~ Bool False)
            & Just

    actualComparisonJson `jsonShouldBe` expectedComparisonJson

  if API._dscSupportsPrimaryKeys _cDataSchema
    then testPerTable "returns the correct primary keys for the Chinook tables" $ \API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> getSchemaGuarded
      let actualPrimaryKey = API._tiPrimaryKey <$> tables
      actualPrimaryKey `jsonShouldBe` Just _tiPrimaryKey
    else testPerTable "returns no primary keys for the Chinook tables" $ \API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> getSchemaGuarded
      let actualPrimaryKey = API._tiPrimaryKey <$> tables
      actualPrimaryKey `jsonShouldBe` Just []

  if API._dscSupportsForeignKeys _cDataSchema
    then testPerTable "returns the correct foreign keys for the Chinook tables" $ \expectedTable@API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> getSchemaGuarded

      -- We compare only the constraints and ignore the constraint names since some agents will have
      -- different constraint names
      let extractConstraintsForComparison table =
            sort . HashMap.elems . API._unForeignKeys $ API._tiForeignKeys table
      let actualConstraints = extractConstraintsForComparison <$> tables
      let expectedConstraints = Just $ extractConstraintsForComparison expectedTable

      actualConstraints `jsonShouldBe` expectedConstraints
    else testPerTable "returns no foreign keys for the Chinook tables" $ \API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> getSchemaGuarded

      let actualJsonConstraints = API._tiForeignKeys <$> tables
      actualJsonConstraints `jsonShouldBe` Just (API.ForeignKeys mempty)
  where
    testPerTable ::
      String ->
      ( forall context m.
        ( MonadThrow m,
          MonadIO m,
          HasBaseContext context,
          HasAgentClient context,
          HasAgentTestContext context,
          HasDatasetContext context
        ) =>
        API.TableInfo ->
        AgentClientT (ExampleT context m) ()
      ) ->
      AgentDatasetTestSpec
    testPerTable description test =
      describe description $ do
        forM_ _tdSchemaTables $ \expectedTable@API.TableInfo {..} -> do
          it (Text.unpack . NonEmpty.last $ API.unTableName _tiName) $
            test expectedTable

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x
