{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Arbitrarily putting leaves from RQL.IR.Select into own files to
-- parallelise compilation
module Hasura.RQL.IR.Select.TablePerm
  ( TablePerm,
    TablePermG (..),
    noTablePermissions,
  )
where

import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType

-- Permissions

data TablePermG (b :: BackendType) v = TablePerm
  { _tpFilter :: AnnBoolExp b v,
    _tpLimit :: (Maybe Int)
  }
  deriving stock (Generic, Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (AnnBoolExp b v)
  ) =>
  Eq (TablePermG b v)

deriving stock instance
  ( Backend b,
    Show (AnnBoolExp b v)
  ) =>
  Show (TablePermG b v)

instance
  ( Backend b,
    Hashable (AnnBoolExp b v)
  ) =>
  Hashable (TablePermG b v)

type TablePerm b = TablePermG b (SQLExpression b)

noTablePermissions :: TablePermG backend v
noTablePermissions = TablePerm annBoolExpTrue Nothing
