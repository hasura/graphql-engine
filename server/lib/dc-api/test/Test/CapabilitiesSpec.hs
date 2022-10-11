module Test.CapabilitiesSpec (spec) where

import Hasura.Backends.DataConnector.API (Capabilities, CapabilitiesResponse (..), Config, Routes (..), capabilitiesCase, validateConfigAgainstConfigSchema)
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Expectations (jsonShouldBe)
import Test.Hspec (Spec, describe, it)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> Config -> Capabilities -> Spec
spec api config expectedCapabilities = describe "capabilities API" $ do
  it "returns the expected capabilities" $ do
    CapabilitiesResponse {..} <- capabilitiesGuard =<< (api // _capabilities)
    _crCapabilities `jsonShouldBe` expectedCapabilities

  it "returns a schema that can be used to validate the current config" $ do
    CapabilitiesResponse {..} <- capabilitiesGuard =<< (api // _capabilities)
    validateConfigAgainstConfigSchema _crConfigSchemaResponse config `jsonShouldBe` []
  where
    defaultAction = fail "Unexpected data connector capabilities response - Unexpected Type"
    errorAction e = fail $ "Unexpected data connector capabilities error response: " <> show e
    capabilitiesGuard = capabilitiesCase defaultAction (pure) errorAction
