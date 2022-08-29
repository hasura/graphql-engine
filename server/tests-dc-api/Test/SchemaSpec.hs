module Test.SchemaSpec (spec) where

--------------------------------------------------------------------------------

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort, sortOn)
import Hasura.Backends.DataConnector.API (Config, Constraint (..), ForeignKeys (..), Routes (..), SchemaResponse (..), SourceName, TableInfo (..))
import Hasura.Backends.DataConnector.API.V0.Column (ColumnInfo (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

--------------------------------------------------------------------------------

removeDescriptionFromColumn :: ColumnInfo -> ColumnInfo
removeDescriptionFromColumn c = c {dciDescription = Nothing}

removeDescription :: TableInfo -> TableInfo
removeDescription t@TableInfo {dtiColumns} = t {dtiDescription = Nothing, dtiColumns = newColumns}
  where
    newColumns = map removeDescriptionFromColumn dtiColumns

removeForeignKeys :: TableInfo -> TableInfo
removeForeignKeys t = t {dtiForeignKeys = Nothing}

extractForeignKeys :: TableInfo -> [Constraint]
extractForeignKeys = foldMap (HashMap.elems . unConstraints) . dtiForeignKeys

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "schema API" $ do
  it "returns Chinook schema" $ do
    tables <- (map removeDescription . sortOn dtiName . srTables) <$> (api // _schema) sourceName config

    -- NOTE: Constraint names arent guaranteed to be the same across
    -- Chinook backends so we compare Constraints without their names
    -- independently from the rest of the schema.
    (map removeForeignKeys tables) `jsonShouldBe` map (removeForeignKeys . removeDescription) Data.schemaTables
    (map (sort . extractForeignKeys) tables) `jsonShouldBe` map (sort . extractForeignKeys) Data.schemaTables
