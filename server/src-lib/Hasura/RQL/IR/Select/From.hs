{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | More leaves peeled from `RQL.IR.Select` for compilation speed
module Hasura.RQL.IR.Select.From
  ( FIIdentifier (..),
    SelectFrom,
    SelectFromG (..),
  )
where

import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Function.Cache
import Hasura.NativeQuery.IR (NativeQuery)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.StoredProcedure.IR (StoredProcedure)

data SelectFromG (b :: BackendType) v
  = FromTable (TableName b)
  | FromIdentifier FIIdentifier -- TODO: Make this into TableIdentifier?
  | FromFunction
      (FunctionName b)
      (FunctionArgsExp b v)
      -- a definition list
      (Maybe [(Column b, ScalarType b)])
  | FromNativeQuery (NativeQuery b v)
  | FromStoredProcedure (StoredProcedure b v)
  deriving stock (Generic)

deriving stock instance (Backend b) => Functor (SelectFromG b)

deriving stock instance (Backend b) => Foldable (SelectFromG b)

deriving stock instance (Backend b) => Traversable (SelectFromG b)

deriving stock instance
  ( Backend b,
    Eq v,
    Eq (FunctionArgumentExp b v),
    Eq (NativeQuery b v),
    Eq (StoredProcedure b v)
  ) =>
  Eq (SelectFromG b v)

deriving stock instance
  ( Backend b,
    Show v,
    Show (FunctionArgumentExp b v),
    Show (NativeQuery b v),
    Show (StoredProcedure b v)
  ) =>
  Show (SelectFromG b v)

instance
  ( Backend b,
    Hashable v,
    Hashable (FunctionArgumentExp b v),
    Hashable (NativeQuery b v),
    Hashable (StoredProcedure b v)
  ) =>
  Hashable (SelectFromG b v)

type SelectFrom b = SelectFromG b (SQLExpression b)

-- | Identifier used exclusively as the argument to 'FromIdentifier'
newtype FIIdentifier = FIIdentifier
  { unFIIdentifier :: Text
  }
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (Hashable)

instance Postgres.IsIdentifier FIIdentifier where
  toIdentifier = coerce
  {-# INLINE toIdentifier #-}
