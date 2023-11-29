{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | More leaves cut from RQL.IR.Select for sake of breaking up the big pile of
-- things
module Hasura.RQL.IR.Select.RelationSelect
  ( AnnRelationSelectG (..),
  )
where

import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.Local (Nullable)

-- Local relationship

data AnnRelationSelectG (b :: BackendType) a = AnnRelationSelectG
  { _aarRelationshipName :: RelName, -- Relationship name
    _aarColumnMapping :: HashMap (ColumnPath b) (ColumnPath b), -- Column of left table to join with
    _aarNullable :: Nullable, -- is the target object allowed to be missing?
    _aarAnnSelect :: a -- Current table. Almost ~ to SQL Select
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance (Backend b, Eq v) => Eq (AnnRelationSelectG b v)

deriving stock instance (Backend b, Show v) => Show (AnnRelationSelectG b v)
