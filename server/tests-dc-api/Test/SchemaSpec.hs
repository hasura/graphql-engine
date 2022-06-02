module Test.SchemaSpec (spec) where

import Data.List (sortOn)
import Hasura.Backends.DataConnector.API (Capabilities, Config, Routes (..), SchemaResponse (..), SourceName, TableInfo (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Capabilities -> Spec
spec api sourceName config expectedCapabilities = describe "schema API" $ do
  it "returns the expected capabilities" $ do
    capabilities <- srCapabilities <$> (api // _schema) sourceName config
    capabilities `shouldBe` expectedCapabilities

  it "returns Chinook schema" $ do
    tables <- (sortOn dtiName . srTables) <$> (api // _schema) sourceName config
    tables `shouldBe` Data.schemaTables
