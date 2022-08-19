module Hasura.SQL.Tag
  ( BackendTag (..),
    HasTag (..),
    reify,
  )
where

import Hasura.SQL.Backend

-- | A singleton-like GADT that associates a tag to each backend.
data BackendTag (b :: BackendType) where
  PostgresVanillaTag :: BackendTag ('Postgres 'Vanilla)
  PostgresCitusTag :: BackendTag ('Postgres 'Citus)
  PostgresCockroachTag :: BackendTag ('Postgres 'Cockroach)
  MSSQLTag :: BackendTag 'MSSQL
  BigQueryTag :: BackendTag 'BigQuery
  MySQLTag :: BackendTag 'MySQL
  DataConnectorTag :: BackendTag 'DataConnector

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

instance HasTag 'MySQL where
  backendTag = MySQLTag

instance HasTag 'DataConnector where
  backendTag = DataConnectorTag

-- | How to convert back from a tag to a runtime value.
reify :: BackendTag b -> BackendType
reify PostgresVanillaTag = Postgres Vanilla
reify PostgresCitusTag = Postgres Citus
reify PostgresCockroachTag = Postgres Cockroach
reify MSSQLTag = MSSQL
reify BigQueryTag = BigQuery
reify MySQLTag = MySQL
reify DataConnectorTag = DataConnector
