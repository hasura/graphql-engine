{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Select.OrderBy
  ( AnnotatedAggregateOrderBy (..),
    AggregateOrderByColumn (..),
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
  = AOCColumn
      (ColumnInfo b)
      -- | This type is used to determine whether the column should be redacted
      -- before being ordered over
      (AnnRedactionExp b v)
  | AOCNestedObject
      (NestedObjectInfo b)
      (AnnotatedOrderByElement b v)
  | AOCObjectRelation
      (RelInfo b)
      -- | Permission filter of the remote table to which the relationship is defined
      (AnnBoolExp b v)
      (AnnotatedOrderByElement b v)
  | AOCArrayAggregation
      (RelInfo b)
      -- | Permission filter of the remote table to which the relationship is defined
      (AnnBoolExp b v)
      (AnnotatedAggregateOrderBy b v)
  | AOCComputedField (ComputedFieldOrderBy b v)
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnBoolExp b v),
    Eq (AnnotatedAggregateOrderBy b v),
    Eq (ComputedFieldOrderBy b v),
    Eq (AnnRedactionExp b v)
  ) =>
  Eq (AnnotatedOrderByElement b v)

deriving stock instance
  ( Backend b,
    Show (AnnBoolExp b v),
    Show (AnnotatedAggregateOrderBy b v),
    Show (ComputedFieldOrderBy b v),
    Show (AnnRedactionExp b v)
  ) =>
  Show (AnnotatedOrderByElement b v)

instance
  ( Backend b,
    Hashable (AnnBoolExp b v),
    Hashable (AnnotatedAggregateOrderBy b v),
    Hashable (ComputedFieldOrderBy b v),
    Hashable (AnnRedactionExp b v)
  ) =>
  Hashable (AnnotatedOrderByElement b v)

data AnnotatedAggregateOrderBy (b :: BackendType) v
  = AAOCount
  | -- | Order by an aggregate function applied to a column
    AAOOp (AggregateOrderByColumn b v)
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance (Backend b, Eq (AggregateOrderByColumn b v)) => Eq (AnnotatedAggregateOrderBy b v)

deriving stock instance (Backend b, Show (AggregateOrderByColumn b v)) => Show (AnnotatedAggregateOrderBy b v)

instance (Backend b, Hashable (AggregateOrderByColumn b v)) => Hashable (AnnotatedAggregateOrderBy b v)

data AggregateOrderByColumn b v = AggregateOrderByColumn
  { _aobcAggregateFunctionName :: Text,
    _aobcAggregateFunctionReturnType :: ColumnType b,
    _aobcColumn :: ColumnInfo b,
    -- | This type is used to determine whether the column should be redacted
    -- before being aggregated and then ordered over
    _aobcRedactionExpression :: AnnRedactionExp b v
  }
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance (Backend b, Eq (AnnRedactionExp b v)) => Eq (AggregateOrderByColumn b v)

deriving stock instance (Backend b, Show (AnnRedactionExp b v)) => Show (AggregateOrderByColumn b v)

instance (Backend b, Hashable (AnnRedactionExp b v)) => Hashable (AggregateOrderByColumn b v)

type AnnotatedOrderByItemG b v = OrderByItemG b (AnnotatedOrderByElement b v)

type AnnotatedOrderByItem b = AnnotatedOrderByItemG b (SQLExpression b)

-- | The order by element for a computed field based on its return type
data ComputedFieldOrderByElement (b :: BackendType) v
  = -- | Sort by the scalar computed field
    CFOBEScalar
      (ScalarType b)
      -- | This type is used to determine whether the computed field should be redacted
      -- before being ordered over
      (AnnRedactionExp b v)
  | CFOBETableAggregation
      (TableName b)
      -- | Permission filter of the retuning table
      (AnnBoolExp b v)
      -- | Sort by aggregation fields of table rows returned by computed field
      (AnnotatedAggregateOrderBy b v)
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnBoolExp b v),
    Eq (AnnotatedAggregateOrderBy b v),
    Eq (AnnRedactionExp b v)
  ) =>
  Eq (ComputedFieldOrderByElement b v)

deriving stock instance
  ( Backend b,
    Show v,
    Show (AnnBoolExp b v),
    Show (AnnotatedAggregateOrderBy b v),
    Show (AnnRedactionExp b v)
  ) =>
  Show (ComputedFieldOrderByElement b v)

instance
  ( Backend b,
    Hashable (AnnBoolExp b v),
    Hashable (AnnotatedAggregateOrderBy b v),
    Hashable (AnnRedactionExp b v)
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
