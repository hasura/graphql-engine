module Test.ConfigSpec (spec) where

import Data.Aeson (toJSON)
import Data.OpenApi.Schema.Validation (validateJSON)
import Hasura.Backends.DataConnector.API (Config, ConfigSchemaResponse (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude

spec :: Config -> ConfigSchemaResponse -> Spec
spec config configSchema = describe "config" $ do
  it "has a valid config" $ do
    validateJSON (_csrOtherSchemas configSchema) (_csrConfigSchema configSchema) (toJSON config) `shouldBe` []
