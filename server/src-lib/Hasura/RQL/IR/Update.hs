module Hasura.RQL.IR.Update
  ( AnnotatedUpdateNode,
    AnnotatedUpdateNodeG (..),
  )
where

import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.IR.Returning
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.SQL.Backend

data AnnotatedUpdateNodeG (b :: BackendType) (r :: BackendType -> Type) v = AnnotatedUpdateNode
  { uqp1Table :: !(TableName b),
    uqp1Where :: !(AnnBoolExp b v, AnnBoolExp b v),
    uqp1Check :: !(AnnBoolExp b v),
    -- | All the backend-specific data related to an update mutation
    uqp1BackendIR :: BackendUpdate b v,
    -- we don't prepare the arguments for returning
    -- however the session variable can still be
    -- converted as desired
    uqp1Output :: !(MutationOutputG b r v),
    uqp1AllCols :: ![ColumnInfo b]
  }
  deriving (Functor, Foldable, Traversable)

type AnnotatedUpdateNode b = AnnotatedUpdateNodeG b (Const Void) (SQLExpression b)
