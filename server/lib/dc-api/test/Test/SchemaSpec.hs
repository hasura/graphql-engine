module Test.SchemaSpec (spec) where

--------------------------------------------------------------------------------

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort, sortOn)
import Hasura.Backends.DataConnector.API qualified as API
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

--------------------------------------------------------------------------------

removeDescriptionFromColumn :: API.ColumnInfo -> API.ColumnInfo
removeDescriptionFromColumn c = c {API._ciDescription = Nothing}

removeDescription :: API.TableInfo -> API.TableInfo
removeDescription t@API.TableInfo {API._tiColumns} = t {API._tiDescription = Nothing, API._tiColumns = newColumns}
  where
    newColumns = map removeDescriptionFromColumn _tiColumns

removeForeignKeys :: API.TableInfo -> API.TableInfo
removeForeignKeys t = t {API._tiForeignKeys = Nothing}

extractForeignKeys :: API.TableInfo -> [API.Constraint]
extractForeignKeys = foldMap (HashMap.elems . API.unConstraints) . API._tiForeignKeys

spec :: Client IO (NamedRoutes API.Routes) -> API.SourceName -> API.Config -> Spec
spec api sourceName config = describe "schema API" $ do
  it "returns Chinook schema" $ do
    tables <- (map removeDescription . sortOn API._tiName . API._srTables) <$> (api // API._schema) sourceName config

    -- NOTE: Constraint names arent guaranteed to be the same across
    -- Chinook backends so we compare Constraints without their names
    -- independently from the rest of the schema.
    (map removeForeignKeys tables) `jsonShouldBe` map (removeForeignKeys . removeDescription) Data.schemaTables
    (map (sort . extractForeignKeys) tables) `jsonShouldBe` map (sort . extractForeignKeys) Data.schemaTables
