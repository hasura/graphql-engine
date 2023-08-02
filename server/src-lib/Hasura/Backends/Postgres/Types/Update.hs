{-# LANGUAGE UndecidableInstances #-}

-- | Postgres Types Update
--
-- This module defines the Update-related IR types specific to Postgres.
module Hasura.Backends.Postgres.Types.Update
  ( UpdateOpExpression (..),
    PgUpdateVariant (..),
    updateVariantIsEmpty,
  )
where

import Hasura.Prelude
import Hasura.RQL.IR.Update.Batch (UpdateBatch (..), updateBatchIsEmpty)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (Postgres))

-- | The various @update operators@ supported by PostgreSQL,
-- i.e. the @_set@, @_inc@ operators that appear in the schema.
--
-- See <https://hasura.io/docs/latest/graphql/core/databases/postgres/mutations/update.html#postgres-update-mutation Update Mutations User docs>
data UpdateOpExpression v
  = UpdateSet v
  | UpdateInc v
  | UpdateAppend v
  | UpdatePrepend v
  | UpdateDeleteKey v
  | UpdateDeleteElem v
  | UpdateDeleteAtPath [v]
  deriving (Functor, Foldable, Traversable, Generic, Data, Show, Eq)

-- | The different 'variants' of updates that the Postgres backend supports.
--
-- This is parameterised over @v@ which enables different phases of IR
-- transformation to maintain the overall structure while enriching/transforming
-- the data at the leaves.
data PgUpdateVariant pgKind v
  = SingleBatch (UpdateBatch ('Postgres pgKind) UpdateOpExpression v)
  | MultipleBatches [UpdateBatch ('Postgres pgKind) UpdateOpExpression v]

deriving stock instance (Eq (UpdateBatch ('Postgres pgKind) UpdateOpExpression v)) => Eq (PgUpdateVariant pgKind v)

deriving stock instance (Show (UpdateBatch ('Postgres pgKind) UpdateOpExpression v)) => Show (PgUpdateVariant pgKind v)

deriving stock instance (Backend ('Postgres pgKind)) => Functor (PgUpdateVariant pgKind)

deriving stock instance (Backend ('Postgres pgKind)) => Foldable (PgUpdateVariant pgKind)

deriving stock instance (Backend ('Postgres pgKind)) => Traversable (PgUpdateVariant pgKind)

-- | Are we updating anything?
updateVariantIsEmpty :: PgUpdateVariant b v -> Bool
updateVariantIsEmpty = \case
  SingleBatch b -> updateBatchIsEmpty b
  MultipleBatches batches -> all updateBatchIsEmpty batches
