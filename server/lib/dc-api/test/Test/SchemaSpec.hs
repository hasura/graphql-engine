module Test.SchemaSpec (spec) where

--------------------------------------------------------------------------------

import Control.Lens ((%~), (.~), (?~))
import Control.Lens.At (at)
import Control.Lens.Lens ((&))
import Control.Monad (forM_)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Lens (_Object)
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Hasura.Backends.DataConnector.API qualified as API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data (TestData (..))
import Test.Expectations (jsonShouldBe)
import Test.Hspec (Expectation, Spec, SpecWith, describe, it)
import Prelude

--------------------------------------------------------------------------------

spec :: TestData -> Client IO (NamedRoutes API.Routes) -> API.SourceName -> API.Config -> API.Capabilities -> Spec
spec TestData {..} api sourceName config API.Capabilities {..} = describe "schema API" $ do
  it "returns the Chinook tables" $ do
    let extractTableNames = sort . fmap API._tiName
    tableNames <- (extractTableNames . API._srTables) <$> (api // API._schema) sourceName config

    let expectedTableNames = extractTableNames _tdSchemaTables
    tableNames `jsonShouldBe` expectedTableNames

  testPerTable "returns the correct columns in the Chinook tables" $ \expectedTable@API.TableInfo {..} -> do
    tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> (api // API._schema) sourceName config

    -- We remove some properties here so that we don't compare them since they vary between agent implementations
    let extractJsonForComparison table =
          let columns = fmap toJSON . sortOn API._ciName $ API._tiColumns table
           in columns & traverse %~ \column ->
                column
                  & _Object . at "type" .~ Nothing -- Types can vary between agents since underlying datatypes can change
                  & _Object . at "description" .~ Nothing -- Descriptions are not supported by all agents
                  -- If the agent only supports nullable columns, we make all columns nullable
    let setExpectedColumnNullability columns =
          if API._dscColumnNullability _cDataSchema == API.OnlyNullableColumns
            then columns & traverse %~ (_Object . at "nullable" ?~ Bool True)
            else columns
    let actualJsonColumns = extractJsonForComparison <$> tables
    let expectedJsonColumns = Just . setExpectedColumnNullability $ extractJsonForComparison expectedTable

    actualJsonColumns `jsonShouldBe` expectedJsonColumns

  if API._dscSupportsPrimaryKeys _cDataSchema
    then testPerTable "returns the correct primary keys for the Chinook tables" $ \API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> (api // API._schema) sourceName config
      let actualPrimaryKey = API._tiPrimaryKey <$> tables
      actualPrimaryKey `jsonShouldBe` Just _tiPrimaryKey
    else testPerTable "returns no primary keys for the Chinook tables" $ \API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> (api // API._schema) sourceName config
      let actualPrimaryKey = API._tiPrimaryKey <$> tables
      actualPrimaryKey `jsonShouldBe` Just []

  if API._dscSupportsForeignKeys _cDataSchema
    then testPerTable "returns the correct foreign keys for the Chinook tables" $ \expectedTable@API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> (api // API._schema) sourceName config

      -- We compare only the constraints and ignore the constraint names since some agents will have
      -- different constraint names
      let extractConstraintsForComparison table =
            sort . HashMap.elems . API.unForeignKeys $ API._tiForeignKeys table
      let actualConstraints = extractConstraintsForComparison <$> tables
      let expectedConstraints = Just $ extractConstraintsForComparison expectedTable

      actualConstraints `jsonShouldBe` expectedConstraints
    else testPerTable "returns no foreign keys for the Chinook tables" $ \API.TableInfo {..} -> do
      tables <- find (\t -> API._tiName t == _tiName) . API._srTables <$> (api // API._schema) sourceName config

      let actualJsonConstraints = API._tiForeignKeys <$> tables
      actualJsonConstraints `jsonShouldBe` Just (API.ForeignKeys mempty)
  where
    testPerTable :: String -> (API.TableInfo -> Expectation) -> SpecWith ()
    testPerTable description test =
      describe description $ do
        forM_ _tdSchemaTables $ \expectedTable@API.TableInfo {..} -> do
          it (Text.unpack . NonEmpty.last $ API.unTableName _tiName) $
            test expectedTable
