-- | Service liveness tests: Confirm that the harness is working
-- properly. If this passes, the rest of the tests will pass.
module ServiceLivenessSpec (spec) where

import Harness.Citus qualified as Citus
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Harness.Mysql qualified as Mysql
import Harness.Postgres qualified as Postgres
import Harness.Sqlserver qualified as Sqlserver
import Harness.State (State, getServer)
import Test.Hspec
import Prelude

spec :: SpecWith State
spec = do
  ignoreSubject do
    it "PostgreSQL liveness" $ shouldReturn Postgres.livenessCheck ()
    it "MySQL liveness" $ shouldReturn Mysql.livenessCheck ()
    it "SQLServer liveness" $ shouldReturn Sqlserver.livenessCheck ()
    it "Citus liveness" $ shouldReturn Citus.livenessCheck ()
  it
    "graphql-engine liveness"
    \state ->
      shouldReturn
        (Http.healthCheck (GraphqlEngine.serverUrl (getServer state)))
        ()
