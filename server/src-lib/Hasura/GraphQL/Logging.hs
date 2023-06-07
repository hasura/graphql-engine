-- |
-- This module holds functions and data types used for logging at the GraphQL
-- layer. In contrast with, logging at the HTTP server layer.
module Hasura.GraphQL.Logging
  ( module Hasura.GraphQL.Logging.QueryLog,
    module Hasura.GraphQL.Logging.ExecutionLog,
  )
where

import Hasura.GraphQL.Logging.ExecutionLog
import Hasura.GraphQL.Logging.QueryLog
