module Test.HealthSpec (spec) where

import Hasura.Backends.DataConnector.API (Config, Routes (..), SourceName)
import Servant.API (NamedRoutes, NoContent (..))
import Servant.Client (Client, (//))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Prelude

spec :: Client IO (NamedRoutes Routes) -> SourceName -> Config -> Spec
spec api sourceName config = describe "health API" $ do
  it "returns a successful HTTP status code for a plain healthcheck" $ do
    response <- (api // _health) Nothing Nothing
    response `shouldBe` NoContent

  it "returns a successful HTTP status code for a data source healthcheck" $ do
    response <- (api // _health) (Just sourceName) (Just config)
    response `shouldBe` NoContent
