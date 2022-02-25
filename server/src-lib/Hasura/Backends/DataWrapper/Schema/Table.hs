module Hasura.Backends.DataWrapper.Schema.Table
  ( Table (..),
  )
where

--------------------------------------------------------------------------------

import Hasura.Backends.DataWrapper.IR.Table qualified as Table (Name)
import Hasura.Backends.DataWrapper.Schema.Column (Column)
import Hasura.Prelude

--------------------------------------------------------------------------------

-- | A schematic representation which captures a named collection of columns
--
-- TODO(cdparks): schematic in the sense of "relating to a schema" or symbolic?
-- This language is also used in the @Column@ documentation
--
-- An element of a table is known as a row, record, tuple, or object,
-- and conforms to the shape specified by the list of @Column@s below.
--
-- cf. https://en.wikipedia.org/wiki/Table_(database)
--     https://www.postgresql.org/docs/13/ddl-basics.html
--
-- NOTE(jkachmar): This type shouldn't _need_ ser/de instances, but they're
-- imposed by the 'Backend' class.
data Table = Table
  { name :: Table.Name,
    columns :: [Column],
    -- TODO(cdparks): Composite primary keys
    primaryKey :: Maybe Text,
    description :: Maybe Text
  }
  deriving stock (Data, Eq, Generic, Ord, Show)
