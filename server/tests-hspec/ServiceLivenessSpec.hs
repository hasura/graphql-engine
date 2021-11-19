-- | Service liveness tests: Confirm that the harness is working
-- properly. If this passes, the rest of the tests will pass.
module ServiceLivenessSpec (spec) where

import Harness.Constants qualified as Constants
import Harness.Http qualified as Http
import Harness.Mysql qualified as Mysql
import Harness.Postgres qualified as Postgres
import Test.Hspec

spec :: Spec
spec = do
  it "PostgreSQL liveness" (shouldReturn Postgres.livenessCheck ())
  it "MySQL liveness" (shouldReturn Mysql.livenessCheck ())
  it
    "graphql-engine liveness"
    (shouldReturn (Http.healthCheck Constants.graphqlEngineUrlPrefix) ())
