module Test.CapabilitiesSpec (spec) where

import Hasura.Backends.DataConnector.API (Capabilities, CapabilitiesResponse (..), Config, Routes (..), validateConfigAgainstConfigSchema)
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Expectations (jsonShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Config -> Capabilities -> Spec
spec api config expectedCapabilities = describe "capabilities API" $ do
  it "returns the expected capabilities" $ do
    CapabilitiesResponse capabilities _ <- api // _capabilities
    capabilities `jsonShouldBe` expectedCapabilities

  it "returns a schema that can be used to validate the current config" $ do
    CapabilitiesResponse _ configSchema <- api // _capabilities
    validateConfigAgainstConfigSchema configSchema config `jsonShouldBe` []
