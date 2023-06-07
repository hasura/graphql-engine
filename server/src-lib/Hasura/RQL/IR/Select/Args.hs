{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Removed from `RQL.IR.Select` to speed up compilation
module Hasura.RQL.IR.Select.Args
  ( SelectArgs,
    SelectArgsG (..),
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
    _ssaCursorArg :: StreamCursorItem b
  }
  deriving (Generic, Functor, Foldable, Traversable)

type SelectStreamArgs b = SelectStreamArgsG b (SQLExpression b)

deriving instance
  ( Backend b,
    Eq (AnnBoolExp b v),
    Eq v
  ) =>
  Eq (SelectStreamArgsG b v)

deriving instance
  ( Backend b,
    Show (AnnBoolExp b v),
    Show v
  ) =>
  Show (SelectStreamArgsG b v)

data SelectArgsG (b :: BackendType) v = SelectArgs
  { _saWhere :: Maybe (AnnBoolExp b v),
    _saOrderBy :: Maybe (NE.NonEmpty (AnnotatedOrderByItemG b v)),
    _saLimit :: Maybe Int,
    _saOffset :: Maybe Int64,
    _saDistinct :: (Maybe (NE.NonEmpty (Column b)))
  }
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnBoolExp b v),
    Eq (AnnotatedOrderByItemG b v)
  ) =>
  Eq (SelectArgsG b v)

instance
  ( Backend b,
    Hashable (AnnBoolExp b v),
    Hashable (AnnotatedOrderByItemG b v)
  ) =>
  Hashable (SelectArgsG b v)

deriving stock instance
  ( Backend b,
    Show (AnnBoolExp b v),
    Show (AnnotatedOrderByItemG b v)
  ) =>
  Show (SelectArgsG b v)

type SelectArgs b = SelectArgsG b (SQLExpression b)

noSelectArgs :: SelectArgsG backend v
noSelectArgs = SelectArgs Nothing Nothing Nothing Nothing Nothing

-- | Cursor for streaming subscription
data StreamCursorItem (b :: BackendType) = StreamCursorItem
  { -- | Specifies how the cursor item should be ordered
    _sciOrdering :: CursorOrdering,
    -- | Column info of the cursor item
    _sciColInfo :: ColumnInfo b,
    -- | Initial value of the cursor item from where the streaming should start
    _sciInitialValue :: ColumnValue b
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (StreamCursorItem b)

deriving instance (Backend b) => Show (StreamCursorItem b)
