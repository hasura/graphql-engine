-- | Service liveness tests: Confirm that the harness is working
-- properly. If this passes, the rest of the tests will pass.
module Test.ServiceLivenessSpec (spec) where

import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Mysql qualified as Mysql
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Http qualified as Http
import Harness.TestEnvironment (TestEnvironment (TestEnvironment, server))
import Test.Hspec
import Prelude

spec :: SpecWith TestEnvironment
spec = do
  ignoreSubject do
    it "PostgreSQL liveness" $ shouldReturn Postgres.livenessCheck ()
    it "MySQL liveness" $ shouldReturn Mysql.livenessCheck ()
    it "SQLServer liveness" $ shouldReturn Sqlserver.livenessCheck ()
    it "Citus liveness" $ shouldReturn Citus.livenessCheck ()
  it
    "graphql-engine liveness"
    \TestEnvironment {server} ->
      shouldReturn
        (Http.healthCheck (GraphqlEngine.serverUrl server))
        ()
