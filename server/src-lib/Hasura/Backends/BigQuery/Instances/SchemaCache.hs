{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This module houses type class instances on the BigQuery backend that relate
-- to the Schema Cache.
module Hasura.Backends.BigQuery.Instances.SchemaCache () where

import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType (BackendType (BigQuery))
import Hasura.RQL.Types.SchemaCacheTypes (GetAggregationPredicatesDeps)

instance (Backend 'BigQuery) => GetAggregationPredicatesDeps 'BigQuery
