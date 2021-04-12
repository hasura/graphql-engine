{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Hasura.RQL.Types.Metadata.Instances (module B) where

import           Hasura.Backends.MSSQL.Instances.Metadata    as B ()
import           Hasura.Backends.Postgres.Instances.Metadata as B ()
import           Hasura.Backends.BigQuery.Instances.Metadata as B ()
