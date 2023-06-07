{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.DataConnector.Adapter.Types.Mutations
  ( BackendInsert (..),
    DataConnectorUpdateVariant (..),
    UpdateOperator (..),
  )
where

import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Prelude
import Hasura.RQL.IR.Update.Batch (UpdateBatch)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (..))

--------------------------------------------------------------------------------

-- | The Data Connector-specific data of an Insert expression. Currently, we don't
-- have any.
--
-- This is parameterised over @v@ which enables different phases of IR
-- transformation to maintain the overall structure while enriching/transforming
-- the data at the leaves.
data BackendInsert v = BackendInsert
  deriving stock (Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

data DataConnectorUpdateVariant v
  = SingleBatch (UpdateBatch 'DataConnector UpdateOperator v)
  | MultipleBatches [UpdateBatch 'DataConnector UpdateOperator v]

deriving stock instance (Backend 'DataConnector) => Functor DataConnectorUpdateVariant

deriving stock instance (Backend 'DataConnector) => Foldable DataConnectorUpdateVariant

deriving stock instance (Backend 'DataConnector) => Traversable DataConnectorUpdateVariant

--------------------------------------------------------------------------------

-- | The operators that are used to mutate specific columns on a table
data UpdateOperator v
  = UpdateSet v
  | UpdateCustomOperator API.UpdateColumnOperatorName v
  deriving stock (Functor, Foldable, Traversable)
