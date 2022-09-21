-- | Provides a data type that holds all of the required implementation details
-- for a backend that supports health checks.
module Hasura.RQL.Types.HealthCheckImplementation (HealthCheckImplementation (..)) where

import Autodocodec (JSONCodec)

data HealthCheckImplementation healthCheckTest = HealthCheckImplementation
  { _hciDefaultTest :: healthCheckTest,
    _hciTestCodec :: JSONCodec healthCheckTest
  }
