module Test.Specs.HealthSpec (spec) where

import Servant.API (NoContent (..))
import Test.AgentAPI (getHealth, getSourceHealth)
import Test.Sandwich (describe, shouldBe)
import Test.TestHelpers (AgentDatasetTestSpec, it)
import Prelude

spec :: AgentDatasetTestSpec
spec = describe "health API" $ do
  it "returns a successful HTTP status code for a plain healthcheck" $ do
    response <- getHealth
    response `shouldBe` NoContent

  it "returns a successful HTTP status code for a data source healthcheck" $ do
    response <- getSourceHealth
    response `shouldBe` NoContent
