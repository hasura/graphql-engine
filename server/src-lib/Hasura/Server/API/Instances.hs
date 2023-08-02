{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Hasura.Server.API.Instances (module B) where

import Hasura.Backends.BigQuery.Instances.API as B ()
import Hasura.Backends.DataConnector.Adapter.API as B ()
import Hasura.Backends.MSSQL.Instances.API as B ()
import Hasura.Backends.Postgres.Instances.API as B ()
