module Test.ConfigSchemaSpec (spec) where

import Hasura.Backends.DataConnector.API (Config, Routes (..), validateConfigAgainstConfigSchema)
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Config -> Spec
spec api config = describe "config schema API" $ do
  it "returns a schema that can be used to validate the current config" $ do
    configSchema <- api // _configSchema
    validateConfigAgainstConfigSchema configSchema config `shouldBe` []
