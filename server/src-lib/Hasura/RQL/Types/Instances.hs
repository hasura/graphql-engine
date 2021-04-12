{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Hasura.RQL.Types.Instances (module B) where

import           Hasura.Backends.MSSQL.Instances.Types    as B ()
import           Hasura.Backends.Postgres.Instances.Types as B ()
import           Hasura.Backends.BigQuery.Instances.Types as B ()
