{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This module houses type class instances on the Postgres backends that relate
-- to the Schema Cache.
module Hasura.Backends.Postgres.Instances.SchemaCache () where

import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.SchemaCacheTypes (GetAggregationPredicatesDeps)
import Hasura.SQL.Backend (BackendType (Postgres))

instance (Backend ('Postgres pgKind)) => GetAggregationPredicatesDeps ('Postgres pgKind)
