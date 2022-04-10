module Test.SchemaSpec (spec) where

import Data.List (sortOn)
import Hasura.Backends.DataWrapper.API (Capabilities, Routes (..), SchemaResponse (..), TableInfo (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Data qualified as Data
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Capabilities -> Spec
spec api expectedCapabilities = describe "schema API" $ do
  it "returns the expected capabilities" $ do
    capabilities <- fmap srCapabilities $ api // _schema
    capabilities `shouldBe` expectedCapabilities

  it "returns Chinook schema" $ do
    tables <- fmap (sortOn dtiName . srTables) $ api // _schema
    tables `shouldBe` Data.schemaTables
