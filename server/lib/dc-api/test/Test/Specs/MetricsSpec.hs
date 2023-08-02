module Test.Specs.MetricsSpec (spec) where

import Hasura.Backends.DataConnector.API (MetricsCapabilities (..))
import Test.AgentAPI (getMetrics)
import Test.Sandwich (describe, shouldNotBe)
import Test.TestHelpers (AgentClientTestSpec, it)
import Prelude

-- NOTE: We currently only check that the response is non-empty, but should probably check that
--       it conforms to the prometheus/openmetrics format at some point in the future.
spec :: MetricsCapabilities -> AgentClientTestSpec
spec MetricsCapabilities = describe "Metrics API" $ do
  it "Returns a response from the /metrics endpoint" $ do
    response <- getMetrics
    response `shouldNotBe` ""
