{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This module houses type class instances on the MySQL backend that relate
-- to the Schema Cache.
module Hasura.Backends.MySQL.Instances.SchemaCache () where

import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType (BackendType (MySQL))
import Hasura.RQL.Types.SchemaCacheTypes (GetAggregationPredicatesDeps)

instance (Backend 'MySQL) => GetAggregationPredicatesDeps 'MySQL
