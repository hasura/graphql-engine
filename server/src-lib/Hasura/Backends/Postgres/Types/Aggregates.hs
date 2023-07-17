{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Types.Aggregates
  ( CountAggregate (..),
  )
where

import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType

-- | This newtype allows us to reuse 'S.CountType' for the 'Backend.CountType' type family
-- We reuse the overall structure, but our column type is a PGCol column name, plus
-- the column censorship expression used by inherited roles.
-- See [SQL generation for inherited roles] for more information about column censorship
newtype CountAggregate pgKind v = CountAggregate {getCountType :: S.CountType (PGCol, Maybe (AnnColumnCaseBoolExp ('Postgres pgKind) v))}
  deriving stock (Generic)

deriving stock instance (Backend ('Postgres pgKind)) => Functor (CountAggregate pgKind)

deriving stock instance (Backend ('Postgres pgKind)) => Foldable (CountAggregate pgKind)

deriving stock instance (Backend ('Postgres pgKind)) => Traversable (CountAggregate pgKind)

deriving stock instance
  (Backend ('Postgres pgKind), Show (AnnColumnCaseBoolExp ('Postgres pgKind) v), Show v) =>
  Show (CountAggregate pgKind v)

deriving stock instance
  (Backend ('Postgres pgKind), Eq (AnnColumnCaseBoolExp ('Postgres pgKind) v), Eq v) =>
  Eq (CountAggregate pgKind v)
