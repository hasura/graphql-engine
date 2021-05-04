{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Hasura.GraphQL.Schema.Instances (module B) where

import           Hasura.Backends.MSSQL.Instances.Schema    as B ()
import           Hasura.Backends.Postgres.Instances.Schema as B ()
import           Hasura.Backends.BigQuery.Instances.Schema as B ()
