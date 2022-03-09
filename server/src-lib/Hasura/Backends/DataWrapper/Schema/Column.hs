module Hasura.Backends.DataWrapper.Schema.Column
  ( Column (..),
  )
where

--------------------------------------------------------------------------------

import Hasura.Backends.DataWrapper.IR.Column qualified as Column (Name)
import Hasura.Backends.DataWrapper.IR.Scalar.Type qualified as Scalar (Type)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | A schematic representation which captures common attributes associated
-- with a piece of data that is stored in a given backend.
--
-- These attributes ascribe meaningful semantics to the data that they are
-- associated with.
--
-- cf. https://en.wikipedia.org/wiki/Column_(database)
--     https://www.postgresql.org/docs/13/ddl-basics.html
--
-- XXX: Instead of an @isNullable@ flag, should we instead add a @Nullable@
-- data constructor to 'Scalar.Type'?
data Column = Column
  { name :: Column.Name,
    type_ :: Scalar.Type,
    isNullable :: Bool,
    description :: Maybe Text
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
