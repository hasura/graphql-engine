{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Select.OrderBy
  ( AnnotatedAggregateOrderBy (..),
    AnnotatedOrderByElement (..),
    AnnotatedOrderByItem,
    AnnotatedOrderByItemG,
    ComputedFieldOrderBy (..),
    ComputedFieldOrderByElement (..),
  )
where

import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.OrderBy
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local

data AnnotatedOrderByElement (b :: BackendType) v
  = AOCColumn (ColumnInfo b)
  | AOCObjectRelation
      (RelInfo b)
      (AnnBoolExp b v)
      -- ^ Permission filter of the remote table to which the relationship is defined
      (AnnotatedOrderByElement b v)
  | AOCArrayAggregation
      (RelInfo b)
      (AnnBoolExp b v)
      -- ^ Permission filter of the remote table to which the relationship is defined
      (AnnotatedAggregateOrderBy b)
  | AOCComputedField (ComputedFieldOrderBy b v)
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnBoolExp b v),
    Eq (AnnotatedAggregateOrderBy b),
    Eq (ComputedFieldOrderBy b v)
  ) =>
  Eq (AnnotatedOrderByElement b v)

deriving stock instance
  ( Backend b,
    Show (AnnBoolExp b v),
    Show (AnnotatedAggregateOrderBy b),
    Show (ComputedFieldOrderBy b v)
  ) =>
  Show (AnnotatedOrderByElement b v)

instance
  ( Backend b,
    Hashable (AnnBoolExp b v),
    Hashable (AnnotatedAggregateOrderBy b),
    Hashable (ComputedFieldOrderBy b v)
  ) =>
  Hashable (AnnotatedOrderByElement b v)

data AnnotatedAggregateOrderBy (b :: BackendType)
  = AAOCount
  | -- | Order by an aggregate function applied to a column
    -- Fields are: Aggregate function name, aggregate function return type, column being aggregated
    AAOOp Text (ColumnType b) (ColumnInfo b)
  deriving stock (Generic)

deriving stock instance (Backend b) => Eq (AnnotatedAggregateOrderBy b)

deriving stock instance (Backend b) => Show (AnnotatedAggregateOrderBy b)

instance (Backend b) => Hashable (AnnotatedAggregateOrderBy b)

type AnnotatedOrderByItemG b v = OrderByItemG b (AnnotatedOrderByElement b v)

type AnnotatedOrderByItem b = AnnotatedOrderByItemG b (SQLExpression b)

-- | The order by element for a computed field based on its return type
data ComputedFieldOrderByElement (b :: BackendType) v
  = -- | Sort by the scalar computed field
    CFOBEScalar (ScalarType b)
  | CFOBETableAggregation
      (TableName b)
      (AnnBoolExp b v)
      -- ^ Permission filter of the retuning table
      (AnnotatedAggregateOrderBy b)
      -- ^ Sort by aggregation fields of table rows returned by computed field
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnBoolExp b v),
    Eq (AnnotatedAggregateOrderBy b)
  ) =>
  Eq (ComputedFieldOrderByElement b v)

deriving stock instance
  ( Backend b,
    Show v,
    Show (AnnBoolExp b v),
    Show (AnnotatedAggregateOrderBy b)
  ) =>
  Show (ComputedFieldOrderByElement b v)

instance
  ( Backend b,
    Hashable (AnnBoolExp b v),
    Hashable (AnnotatedAggregateOrderBy b)
  ) =>
  Hashable (ComputedFieldOrderByElement b v)

data ComputedFieldOrderBy (b :: BackendType) v = ComputedFieldOrderBy
  { _cfobXField :: XComputedField b,
    _cfobName :: ComputedFieldName,
    _cfobFunction :: FunctionName b,
    _cfobFunctionArgsExp :: FunctionArgsExp b v,
    _cfobOrderByElement :: ComputedFieldOrderByElement b v
  }
  deriving stock (Generic)

deriving stock instance (Backend b) => Functor (ComputedFieldOrderBy b)

deriving stock instance (Backend b) => Foldable (ComputedFieldOrderBy b)

deriving stock instance (Backend b) => Traversable (ComputedFieldOrderBy b)

deriving stock instance
  ( Backend b,
    Eq (ComputedFieldOrderByElement b v),
    Eq (FunctionArgsExp b v)
  ) =>
  Eq (ComputedFieldOrderBy b v)

deriving stock instance
  ( Backend b,
    Show (ComputedFieldOrderByElement b v),
    Show (FunctionArgsExp b v)
  ) =>
  Show (ComputedFieldOrderBy b v)

instance
  ( Backend b,
    Hashable (ComputedFieldOrderByElement b v),
    Hashable (FunctionArgsExp b v)
  ) =>
  Hashable (ComputedFieldOrderBy b v)
