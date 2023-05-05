{-# OPTIONS_GHC -Wno-dodgy-exports #-}

--

module Hasura.RQL.Types.SchemaCache.Instances (module I) where

import Hasura.Backends.BigQuery.Instances.SchemaCache as I ()
import Hasura.Backends.DataConnector.Adapter.SchemaCache as I ()
import Hasura.Backends.MSSQL.Instances.SchemaCache as I ()
import Hasura.Backends.Postgres.Instances.SchemaCache as I ()
