{-# LANGUAGE UndecidableInstances #-}

-- | Contains types that can be used by backends to structure updates
-- to batches of rows in a table
module Hasura.RQL.IR.Update.Batch
  ( UpdateBatch (..),
    updateBatchIsEmpty,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType

-- | Represents a set of update operations ('_ubOperations') applied to a batch of rows selected
-- from a table by filtering it with a boolean expression ('_ubWhere').
--
-- This type may be used by specific backends as a part their 'UpdateVariant'.
-- See 'Hasura.Backends.Postgres.Types.Update.PgUpdateVariant' for an example.
--
-- The actual operators used to affect changes against columns in '_ubOperations' are abstract
-- here and are specified by the specific backends based on what they actually support
data UpdateBatch (b :: BackendType) updateOperators v = UpdateBatch
  { _ubOperations :: HashMap (Column b) (updateOperators v),
    _ubWhere :: AnnBoolExp b v
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Show v,
    Show (updateOperators v),
    Show (AnnBoolExp b v)
  ) =>
  Show (UpdateBatch b updateOperators v)

deriving stock instance
  ( Backend b,
    Eq v,
    Eq (updateOperators v),
    Eq (AnnBoolExp b v)
  ) =>
  Eq (UpdateBatch b updateOperators v)

-- | Are we actually updating anything in the batch?
updateBatchIsEmpty :: UpdateBatch b updateOperators v -> Bool
updateBatchIsEmpty UpdateBatch {..} =
  HashMap.null _ubOperations
