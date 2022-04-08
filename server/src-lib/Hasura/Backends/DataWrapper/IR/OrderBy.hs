{-# LANGUAGE DeriveAnyClass #-}

module Hasura.Backends.DataWrapper.IR.OrderBy
  ( OrderBy (..),
    OrderType (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (ToJSON)
import Data.Aeson qualified as J
import Hasura.Backends.DataWrapper.API qualified as API
import Hasura.Backends.DataWrapper.IR.Column qualified as Column (Name)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch

--------------------------------------------------------------------------------

-- | Indicates a particular sort order that should be applied based on some
-- 'Column.Name' returned within a data source query.
--
-- TODO: We should use a sum type like @Query.Field@ here so that we can handle
-- @order by@ constraints on object/array relationships as well.
--
-- cf. https://www.postgresql.org/docs/13/queries-order.html
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
data OrderBy = OrderBy
  { column :: Column.Name,
    ordering :: OrderType
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderBy where
  toJSON = J.genericToJSON J.defaultOptions

instance From API.OrderBy OrderBy where
  from API.OrderBy {column, ordering} =
    OrderBy (from column) (from ordering)

instance From OrderBy API.OrderBy where
  from OrderBy {column, ordering} =
    API.OrderBy (from column) (from ordering)

--------------------------------------------------------------------------------

-- | 'Column.Name's may be sorted in either ascending or descending order.
--
-- cf. https://www.postgresql.org/docs/13/queries-order.html
--
-- NOTE: The 'ToJSON' instance is only intended for logging purposes.
data OrderType
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderType where
  toJSON = J.genericToJSON J.defaultOptions

instance From API.OrderType OrderType where
  from API.Ascending = Ascending
  from API.Descending = Ascending

instance From OrderType API.OrderType where
  from Ascending = API.Ascending
  from Descending = API.Ascending
