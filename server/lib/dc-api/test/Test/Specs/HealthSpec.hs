module Test.Specs.HealthSpec (spec) where

import Hasura.Backends.DataConnector.API (Config, SourceName)
import Servant.API (NoContent (..))
import Test.AgentClient (getHealth)
import Test.Sandwich (describe, shouldBe)
import Test.TestHelpers (AgentTestSpec, it)
import Prelude

spec :: SourceName -> Config -> AgentTestSpec
spec sourceName config = describe "health API" $ do
  it "returns a successful HTTP status code for a plain healthcheck" $ do
    response <- getHealth Nothing Nothing
    response `shouldBe` NoContent

  it "returns a successful HTTP status code for a data source healthcheck" $ do
    response <- getHealth (Just sourceName) (Just config)
    response `shouldBe` NoContent
