module Test.Specs.ConfigSpec (spec) where

import Data.Aeson (toJSON)
import Data.OpenApi.Schema.Validation (validateJSON)
import Hasura.Backends.DataConnector.API (Config, ConfigSchemaResponse (..))
import Test.Sandwich (TopSpec, describe, it, shouldBe)
import Prelude

spec :: Config -> ConfigSchemaResponse -> TopSpec
spec config configSchema = describe "config" $ do
  it "has a valid config" $ do
    validateJSON (_csrOtherSchemas configSchema) (_csrConfigSchema configSchema) (toJSON config) `shouldBe` []
