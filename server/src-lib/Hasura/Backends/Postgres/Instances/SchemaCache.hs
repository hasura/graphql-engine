{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This module houses type class instances on the Postgres backends that relate
-- to the Schema Cache.
module Hasura.Backends.Postgres.Instances.SchemaCache () where

import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType (BackendType (Postgres))
import Hasura.RQL.Types.SchemaCache.AggregationPredicates (defaultGetAggregationPredicateDeps)
import Hasura.RQL.Types.SchemaCacheTypes (GetAggregationPredicatesDeps (..))

instance (Backend ('Postgres pgKind)) => GetAggregationPredicatesDeps ('Postgres pgKind) where
  getAggregationPredicateDeps = defaultGetAggregationPredicateDeps
