{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.BackendTag
  ( BackendTag (..),
    HasTag (..),
    reify,
    backendPrefix,
  )
where

import Data.GADT.Compare.TH
import Data.Text qualified as T
import Data.Text.Extended qualified as T (toTxt)
import Hasura.Prelude
import Hasura.RQL.Types.BackendType

-- | A singleton-like GADT that associates a tag to each backend.
data BackendTag (b :: BackendType) where
  PostgresVanillaTag :: BackendTag ('Postgres 'Vanilla)
  PostgresCitusTag :: BackendTag ('Postgres 'Citus)
  PostgresCockroachTag :: BackendTag ('Postgres 'Cockroach)
  MSSQLTag :: BackendTag 'MSSQL
  BigQueryTag :: BackendTag 'BigQuery
  DataConnectorTag :: BackendTag 'DataConnector

-- Derive GEq and GCompare instances for BackendTag.
-- These are used to write a Select instance for BackendMap.
$(deriveGEq ''BackendTag)
$(deriveGCompare ''BackendTag)

-- | This class describes how to get a tag for a given type.
-- We use it in AnyBackend: `case backendTag @b of`...
class HasTag (b :: BackendType) where
  backendTag :: BackendTag b

instance HasTag ('Postgres 'Vanilla) where
  backendTag = PostgresVanillaTag

instance HasTag ('Postgres 'Citus) where
  backendTag = PostgresCitusTag

instance HasTag ('Postgres 'Cockroach) where
  backendTag = PostgresCockroachTag

instance HasTag 'MSSQL where
  backendTag = MSSQLTag

instance HasTag 'BigQuery where
  backendTag = BigQueryTag

instance HasTag 'DataConnector where
  backendTag = DataConnectorTag

-- | How to convert back from a tag to a runtime value.
reify :: BackendTag b -> BackendType
reify PostgresVanillaTag = Postgres Vanilla
reify PostgresCitusTag = Postgres Citus
reify PostgresCockroachTag = Postgres Cockroach
reify MSSQLTag = MSSQL
reify BigQueryTag = BigQuery
reify DataConnectorTag = DataConnector

-- | Provides a title-cased name for a database kind, inferring the appropriate
-- database kind from type context.
backendPrefix :: forall b. (HasTag b) => Text
backendPrefix = T.toTitle $ T.toTxt $ reify $ backendTag @b
