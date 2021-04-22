{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Hasura.GraphQL.Transport.Instances (module B) where

import           Hasura.Backends.MSSQL.Instances.Transport    as B ()
import           Hasura.Backends.Postgres.Instances.Transport as B ()
import           Hasura.Backends.BigQuery.Instances.Transport as B ()
