module Test.SchemaSpec (spec) where

import Data.List (sortOn)
import Hasura.Backends.DataConnector.API (Config, Routes (..), SchemaResponse (..), SourceName, TableInfo (..))
import Hasura.Backends.DataConnector.API.V0.Column (ColumnInfo (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Expectations (jsonShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

removeDescriptionFromColumn :: ColumnInfo -> ColumnInfo
removeDescriptionFromColumn c = c {dciDescription = Nothing}

removeDescription :: TableInfo -> TableInfo
removeDescription t@TableInfo {dtiColumns} = t {dtiDescription = Nothing, dtiColumns = newColumns}
  where
    newColumns = map removeDescriptionFromColumn dtiColumns

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "schema API" $ do
  it "returns Chinook schema" $ do
    tables <- (map removeDescription . sortOn dtiName . srTables) <$> (api // _schema) sourceName config
    tables `jsonShouldBe` map removeDescription Data.schemaTables
