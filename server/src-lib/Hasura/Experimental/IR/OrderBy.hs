{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Experimental.IR.OrderBy
  ( OrderBy (..),
    OrderType (..),
  )
where

--------------------------------------------------------------------------------

import Hasura.Experimental.IR.Column qualified as Column (Name)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | Indicates a particular sort order that should be applied based on some
-- 'Column.Name' returned within a data source query.
--
-- TODO: We should use a sum type like @Query.Field@ here so that we can handle
-- @order by@ constraints on object/array relationships as well.
--
-- cf. https://www.postgresql.org/docs/13/queries-order.html
data OrderBy = OrderBy
  { column :: Column.Name,
    ordering :: OrderType
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

--------------------------------------------------------------------------------

-- | 'Column.Name's may be sorted in either ascending or descending order.
--
-- cf. https://www.postgresql.org/docs/13/queries-order.html
data OrderType
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)
