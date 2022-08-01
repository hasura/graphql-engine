{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Update
  ( AnnotatedUpdate,
    AnnotatedUpdateG (..),
    auTable,
    auWhere,
    auCheck,
    auBackend,
    auOutput,
    auAllCols,
    auNamingConvention,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Kind (Type)
import Hasura.GraphQL.Schema.NamingCase (NamingCase)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend

data AnnotatedUpdateG (b :: BackendType) (r :: Type) v = AnnotatedUpdateG
  { _auTable :: TableName b,
    -- | The where clause for /update_table/ and /update_table_by_pk/ along with
    -- the permissions filter.
    -- In the case of /update_table_many/, this will be empty and the actual
    -- where clauses (one per update) are found in 'BackendUpdate'.
    _auWhere :: (AnnBoolExp b v, AnnBoolExp b v),
    _auCheck :: AnnBoolExp b v,
    -- | All the backend-specific data related to an update mutation
    _auBackend :: BackendUpdate b v,
    -- we don't prepare the arguments for returning
    -- however the session variable can still be
    -- converted as desired

    -- | Selection set
    _auOutput :: MutationOutputG b r v,
    _auAllCols :: [ColumnInfo b],
    _auNamingConvention :: Maybe NamingCase
  }
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Eq (BooleanOperators b v),
    Eq (BackendUpdate b v),
    Eq (FunctionArgumentExp b v),
    Eq r,
    Eq v
  ) =>
  Eq (AnnotatedUpdateG b r v)

deriving stock instance
  ( Backend b,
    Show (BooleanOperators b v),
    Show (BackendUpdate b v),
    Show (FunctionArgumentExp b v),
    Show r,
    Show v
  ) =>
  Show (AnnotatedUpdateG b r v)

type AnnotatedUpdate b = AnnotatedUpdateG b Void (SQLExpression b)

$(makeLenses ''AnnotatedUpdateG)
