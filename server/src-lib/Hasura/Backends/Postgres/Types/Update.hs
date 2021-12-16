-- | This module defines the Update-related IR types specific to Postgres.
module Hasura.Backends.Postgres.Types.Update
  ( BackendUpdate (..),
    UpdOpExpG (..),
  )
where

import Hasura.Backends.Postgres.SQL.Types (PGCol)
import Hasura.Prelude

-- | The PostgreSQL-specific data of an Update expression.
--
-- This is parameterised over @v@ which enables different phases of IR
-- transformation to maintain the overall structure while enriching/transforming
-- the data at the leaves.
data BackendUpdate v = BackendUpdate
  { -- | The update operations to perform on each colum.
    updateOperations :: !(HashMap PGCol (UpdOpExpG v))
  }
  deriving (Functor, Foldable, Traversable, Generic, Data)

-- | The various @update operators@ supported by PostgreSQL,
-- i.e. the @_set@, @_inc@ operators that appear in the schema.
--
-- See <https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html#postgres-update-mutation Update Mutations User docs>
data UpdOpExpG v
  = UpdSet !v
  | UpdInc !v
  | UpdAppend !v
  | UpdPrepend !v
  | UpdDeleteKey !v
  | UpdDeleteElem !v
  | UpdDeleteAtPath ![v]
  deriving (Functor, Foldable, Traversable, Generic, Data)
