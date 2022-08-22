{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This module houses type class instances on the DataConnector backends that relate
-- to the Schema Cache.
module Hasura.Backends.DataConnector.Adapter.SchemaCache () where

import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.SchemaCacheTypes (GetAggregationPredicatesDeps)
import Hasura.SQL.Backend (BackendType (DataConnector))

instance (Backend 'DataConnector) => GetAggregationPredicatesDeps 'DataConnector
