{-# LANGUAGE DuplicateRecordFields #-}

-- | Removed from `RQL.IR.Select` to speed up compilation
module Hasura.RQL.IR.Select.Args
  ( SelectArgs,
    SelectArgsG (..),
    AnnDistinctColumn (..),
    SelectStreamArgsG (..),
    SelectStreamArgs,
    StreamCursorItem (..),
    noSelectArgs,
  )
where

import Data.Int (Int64)
import Data.List.NonEmpty qualified as NE
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Select.OrderBy
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Subscription

-- Select arguments

data SelectStreamArgsG (b :: BackendType) v = SelectStreamArgsG
  { -- | optional filter to filter the stream results
    _ssaWhere :: Maybe (AnnBoolExp b v),
    -- | maximum number of rows to be returned in a single fetch
    _ssaBatchSize :: Int,
    -- | info related to the cursor column, a single item data type
    --   currently because only single column cursors are supported
    _ssaCursorArg :: StreamCursorItem b v
  }
  deriving (Generic, Functor, Foldable, Traversable)

type SelectStreamArgs b = SelectStreamArgsG b (SQLExpression b)

deriving instance
  ( Backend b,
    Eq v
  ) =>
  Eq (SelectStreamArgsG b v)

deriving instance
  ( Backend b,
    Show v
  ) =>
  Show (SelectStreamArgsG b v)

data SelectArgsG (b :: BackendType) v = SelectArgs
  { _saWhere :: Maybe (AnnBoolExp b v),
    _saOrderBy :: Maybe (NE.NonEmpty (AnnotatedOrderByItemG b v)),
    _saLimit :: Maybe Int,
    _saOffset :: Maybe Int64,
    _saDistinct :: (Maybe (NE.NonEmpty (AnnDistinctColumn b v)))
  }
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq v
  ) =>
  Eq (SelectArgsG b v)

instance
  ( Backend b,
    Hashable v
  ) =>
  Hashable (SelectArgsG b v)

deriving stock instance
  ( Backend b,
    Show v
  ) =>
  Show (SelectArgsG b v)

type SelectArgs b = SelectArgsG b (SQLExpression b)

noSelectArgs :: SelectArgsG backend v
noSelectArgs = SelectArgs Nothing Nothing Nothing Nothing Nothing

data AnnDistinctColumn b v = AnnDistinctColumn
  { _adcColumn :: Column b,
    -- | This type is used to determine whether the column should be redacted
    -- before being distincted-upon
    _adcRedactionExpression :: AnnRedactionExp b v
  }
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq v
  ) =>
  Eq (AnnDistinctColumn b v)

instance
  ( Backend b,
    Hashable v
  ) =>
  Hashable (AnnDistinctColumn b v)

deriving stock instance
  ( Backend b,
    Show v
  ) =>
  Show (AnnDistinctColumn b v)

-- | Cursor for streaming subscription
data StreamCursorItem (b :: BackendType) v = StreamCursorItem
  { -- | Specifies how the cursor item should be ordered
    _sciOrdering :: CursorOrdering,
    -- | Column info of the cursor item
    _sciColInfo :: ColumnInfo b,
    -- | This type is used to determine whether the column should be redacted
    -- before it is sorted upon
    _sciRedactionExpression :: AnnRedactionExp b v,
    -- | Initial value of the cursor item from where the streaming should start
    _sciInitialValue :: ColumnValue b
  }
  deriving (Generic, Functor, Foldable, Traversable)

deriving instance (Backend b, Eq v) => Eq (StreamCursorItem b v)

deriving instance (Backend b, Show v) => Show (StreamCursorItem b v)
