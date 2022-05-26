-- | Postgres Types Update
--
-- This module defines the Update-related IR types specific to Postgres.
module Hasura.Backends.Postgres.Types.Update
  ( BackendUpdate (..),
    UpdateOpExpression (..),
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
    updateOperations :: !(HashMap PGCol (UpdateOpExpression v))
  }
  deriving (Functor, Foldable, Traversable, Generic, Data, Show, Eq)

-- | The various @update operators@ supported by PostgreSQL,
-- i.e. the @_set@, @_inc@ operators that appear in the schema.
--
-- See <https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html#postgres-update-mutation Update Mutations User docs>
data UpdateOpExpression v
  = UpdateSet !v
  | UpdateInc !v
  | UpdateAppend !v
  | UpdatePrepend !v
  | UpdateDeleteKey !v
  | UpdateDeleteElem !v
  | UpdateDeleteAtPath ![v]
  deriving (Functor, Foldable, Traversable, Generic, Data, Show, Eq)
