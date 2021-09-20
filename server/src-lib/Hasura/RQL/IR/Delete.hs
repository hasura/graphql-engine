module Hasura.RQL.IR.Delete where

import           Hasura.Prelude

import           Data.Kind                (Type)

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend


data AnnDelG (b :: BackendType) (r :: BackendType -> Type) v
  = AnnDel
  { dqp1Table   :: !(TableName b)
  , dqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , dqp1Output  :: !(MutationOutputG b r v)
  , dqp1AllCols :: ![ColumnInfo b]
  } deriving (Functor, Foldable, Traversable)

type AnnDel b = AnnDelG b (Const Void) (SQLExpression b)
