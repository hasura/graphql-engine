{-# LANGUAGE ConstraintKinds #-}

module Test.Specs.SchemaSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((%~), (.~), (<&>), (?~))
import Control.Lens.At (at)
import Control.Lens.Lens ((&))
import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (_Object)
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)
import Hasura.Backends.DataConnector.API qualified as API
import Test.AgentAPI (getSchemaGuarded, getSchemaGuarded')
import Test.AgentClient (HasAgentClient, runAgentClientT)
import Test.AgentDatasets (HasDatasetContext)
import Test.AgentTestContext (HasAgentTestContext)
import Test.Data (TestData (..))
import Test.Expectations (jsonShouldBe)
import Test.Sandwich (ExampleT, HasLabel, Label (..), LabelValue, SpecFree, describe, getContext, introduce, (:>))
import Test.Sandwich qualified as Sandwich
import Test.Sandwich.Misc (HasBaseContext)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

--------------------------------------------------------------------------------

spec :: TestData -> API.Capabilities -> AgentDatasetTestSpec
spec TestData {..} API.Capabilities {..} = describe "schema API" $ preloadAgentSchema $ do
  let supportsInserts = isJust $ _cMutations >>= API._mcInsertCapabilities
  let supportsUpdates = isJust $ _cMutations >>= API._mcUpdateCapabilities
  let supportsDeletes = isJust $ _cMutations >>= API._mcDeleteCapabilities

  it "returns the Chinook tables" $ do
    let extractTableNames = sort . fmap API._tiName
    tableNames <- (extractTableNames . API._srTables) <$> getPreloadedAgentSchema

    let expectedTableNames = extractTableNames _tdSchemaTables
    tableNames `jsonShouldBe` expectedTableNames

  it "returns the specified Chinook tables when filtered" $ do
    let desiredTables = [_tdCustomersTableName, _tdInvoicesTableName, _tdInvoiceLinesTableName, _tdTracksTableName]
    let filters = mempty {API._sfOnlyTables = Just desiredTables}
    tableNames <- sort . fmap API._tiName . API._srTables <$> getSchemaGuarded' (API.SchemaRequest filters API.Everything)

    tableNames `jsonShouldBe` desiredTables

  it "returns no tables when filtered with an empty list" $ do
    let filters = mempty {API._sfOnlyTables = Just []}
    tableInfos <- API._srTables <$> getSchemaGuarded' (API.SchemaRequest filters API.Everything)

    tableInfos `jsonShouldBe` []

  it "returns only Chinook table names and types when using basic_info detail level" $ do
    tableInfos <- sortOn API._tiName . API._srTables <$> getSchemaGuarded' (API.SchemaRequest mempty API.BasicInfo)

    let expectedTableInfos =
          _tdSchemaTables
            <&> (\API.TableInfo {..} -> API.TableInfo _tiName _tiType [] Nothing (API.ForeignKeys mempty) Nothing False False False)
            & sortOn API._tiName

    tableInfos `jsonShouldBe` expectedTableInfos

  it "can filter tables while using basic_info detail level" $ do
    let desiredTables = [_tdAlbumsTableName, _tdArtistsTableName]
    let filters = mempty {API._sfOnlyTables = Just desiredTables}
    tableInfos <- sortOn API._tiName . API._srTables <$> getSchemaGuarded' (API.SchemaRequest filters API.BasicInfo)

    let expectedTableInfos =
          _tdSchemaTables
            & mapMaybe
              ( \API.TableInfo {..} ->
                  if _tiName `elem` desiredTables
                    then Just $ API.TableInfo _tiName _tiType [] Nothing (API.ForeignKeys mempty) Nothing False False False
                    else Nothing
              )
            & sortOn API._tiName

    tableInfos `jsonShouldBe` expectedTableInfos

  testPerTable "returns the correct columns in the Chinook tables" $ \expectedTable actualTable -> do
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
            -- If agent doesn't support update mutations then all columns won't be generated
            -- This is a bit of a dubious assumption since it's possible a database supports
            -- generated values but the agent just doesn't support mutations on that DB
            & applyWhen
              (isNothing _cMutations)
              (traverse %~ (_Object . at "value_generated" .~ Nothing))
            & Just

    actualJsonColumns `jsonShouldBe` expectedJsonColumns

  testPerTable "returns the correct mutability in the Chinook tables" $ \expectedTable actualTable -> do
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
    then testPerTable "returns the correct primary keys for the Chinook tables" $ \API.TableInfo {..} actualTable -> do
      let actualPrimaryKey = API._tiPrimaryKey <$> actualTable
      actualPrimaryKey `jsonShouldBe` Just _tiPrimaryKey
    else testPerTable "returns no primary keys for the Chinook tables" $ \_expectedTable actualTable -> do
      let actualPrimaryKey = API._tiPrimaryKey <$> actualTable
      actualPrimaryKey `jsonShouldBe` Nothing

  if API._dscSupportsForeignKeys _cDataSchema
    then testPerTable "returns the correct foreign keys for the Chinook tables" $ \expectedTable actualTable -> do
      -- We compare only the constraints and ignore the constraint names since some agents will have
      -- different constraint names
      let extractConstraintsForComparison table =
            sort . HashMap.elems . API._unForeignKeys $ API._tiForeignKeys table
      let actualConstraints = extractConstraintsForComparison <$> actualTable
      let expectedConstraints = Just $ extractConstraintsForComparison expectedTable

      actualConstraints `jsonShouldBe` expectedConstraints
    else testPerTable "returns no foreign keys for the Chinook tables" $ \_expectedTable actualTable -> do
      let actualJsonConstraints = API._tiForeignKeys <$> actualTable
      actualJsonConstraints `jsonShouldBe` Just (API.ForeignKeys mempty)
  where
    testPerTable ::
      String ->
      ( forall innerContext m.
        ( MonadThrow m,
          MonadIO m
        ) =>
        API.TableInfo -> -- Expected table
        Maybe API.TableInfo -> -- Actual table
        ExampleT innerContext m ()
      ) ->
      forall context. (HasPreloadedAgentSchema context) => SpecFree context IO ()
    testPerTable description test =
      describe description $ do
        forM_ _tdSchemaTables $ \expectedTable@API.TableInfo {..} -> do
          Sandwich.it (Text.unpack . NonEmpty.last $ API.unTableName _tiName) $ do
            schemaResponse <- getPreloadedAgentSchema
            let actualTable = find (\t -> API._tiName t == _tiName) $ API._srTables schemaResponse
            test expectedTable actualTable

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x

preloadAgentSchema :: forall context m. (MonadIO m, MonadThrow m, HasAgentClient context, HasBaseContext context, HasAgentTestContext context, HasDatasetContext context) => SpecFree (LabelValue "agent-schema" API.SchemaResponse :> context) m () -> SpecFree context m ()
preloadAgentSchema = introduce "Preload agent schema" agentSchemaLabel getAgentSchema (const $ pure ())
  where
    getAgentSchema = runAgentClientT Nothing $ getSchemaGuarded

agentSchemaLabel :: Label "agent-schema" API.SchemaResponse
agentSchemaLabel = Label

type HasPreloadedAgentSchema context = HasLabel context "agent-schema" API.SchemaResponse

getPreloadedAgentSchema :: (HasCallStack, HasPreloadedAgentSchema context, MonadReader context m) => m API.SchemaResponse
getPreloadedAgentSchema = getContext agentSchemaLabel
