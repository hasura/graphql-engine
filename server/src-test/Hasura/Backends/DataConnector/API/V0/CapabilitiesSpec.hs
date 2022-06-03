{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Hasura.Backends.DataConnector.API.V0.CapabilitiesSpec (spec) where

import Data.Aeson.QQ.Simple (aesonQQ)
import Hasura.Backends.DataConnector.API.V0.Capabilities
import Hasura.Backends.DataConnector.API.V0.ConfigSchema
import Hasura.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Aeson.Utils
import Test.Hspec

spec :: Spec
spec = do
  describe "Capabilities" $ do
    testToFromJSONToSchema (Capabilities False) [aesonQQ|{"relationships": false}|]
    jsonOpenApiProperties genCapabilities
  describe "CapabilitiesResponse" $ do
    testToFromJSON
      (CapabilitiesResponse (Capabilities True) emptyConfigSchemaResponse)
      [aesonQQ|{"capabilities": {"relationships": true}, "configSchemas": {"configSchema": {}, "otherSchemas": {}}}|]

genCapabilities :: MonadGen m => m Capabilities
genCapabilities = Capabilities <$> Gen.bool

emptyConfigSchemaResponse :: ConfigSchemaResponse
emptyConfigSchemaResponse = ConfigSchemaResponse mempty mempty
