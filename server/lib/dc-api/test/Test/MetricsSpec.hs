module Test.MetricsSpec (spec) where

import Hasura.Backends.DataConnector.API (MetricsCapabilities (..), Routes (..))
import Servant.API (NamedRoutes)
import Servant.Client (Client, (//))
import Test.Hspec (Spec, describe, it, shouldNotBe)
import Prelude

-- NOTE: We currently only check that the response is non-empty, but should probably check that
--       it conforms to the prometheus/openmetrics format at some point in the future.
spec :: Client IO (NamedRoutes Routes) -> MetricsCapabilities -> Spec
spec api MetricsCapabilities = describe "Metrics API" $ do
  it "Returns a response from the /metrics endpoint" $ do
    response <- (api // _metrics)
    response `shouldNotBe` ""
