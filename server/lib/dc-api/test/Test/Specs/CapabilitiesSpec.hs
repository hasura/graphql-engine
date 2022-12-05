module Test.Specs.CapabilitiesSpec (spec) where

import Hasura.Backends.DataConnector.API (Capabilities, CapabilitiesResponse (..), Config, validateConfigAgainstConfigSchema)
import Test.AgentClient (getCapabilitiesGuarded)
import Test.Expectations (jsonShouldBe)
import Test.Sandwich (describe)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

spec :: Config -> Capabilities -> AgentTestSpec
spec config expectedCapabilities = describe "capabilities API" $ do
  it "returns the expected capabilities" $ do
    CapabilitiesResponse {..} <- getCapabilitiesGuarded
    _crCapabilities `jsonShouldBe` expectedCapabilities

  it "returns a schema that can be used to validate the current config" $ do
    CapabilitiesResponse {..} <- getCapabilitiesGuarded
    validateConfigAgainstConfigSchema _crConfigSchemaResponse config `jsonShouldBe` []
