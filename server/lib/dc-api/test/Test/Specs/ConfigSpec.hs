module Test.Specs.ConfigSpec (spec) where

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (toJSON)
import Data.OpenApi.Schema.Validation (validateJSON)
import Hasura.Backends.DataConnector.API (Config, ConfigSchemaResponse (..))
import Test.Sandwich (Spec, describe, it, shouldBe)
import Prelude

spec :: MonadThrow m => Config -> ConfigSchemaResponse -> Spec context m
spec config configSchema = describe "config" $ do
  it "has a valid config" $ do
    validateJSON (_csrOtherSchemas configSchema) (_csrConfigSchema configSchema) (toJSON config) `shouldBe` []
