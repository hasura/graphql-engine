-- | Service liveness tests: Confirm that the harness is working
-- properly. If this passes, the rest of the tests will pass.
module Test.ServiceLivenessSpec (spec) where

import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Harness.TestEnvironment (TestEnvironment (TestEnvironment, server))
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith TestEnvironment
spec = do
  ignoreSubject do
    it "Postgres" $ shouldReturn Postgres.livenessCheck ()
    it "MySQL" $ shouldReturn Mysql.livenessCheck ()
    it "SQLServer" $ shouldReturn Sqlserver.livenessCheck ()
    it "Citus" $ shouldReturn Citus.livenessCheck ()
    it "Cockroach" $ shouldReturn Cockroach.livenessCheck ()
  it
    "graphql-engine"
    \TestEnvironment {server} ->
      shouldReturn
        (Http.healthCheck (GraphqlEngine.serverUrl server))
        ()
