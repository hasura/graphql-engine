module Hasura.RQL.IR.Update where

import           Hasura.Prelude

import           Data.Kind                (Type)

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend


data AnnUpdG (b :: BackendType) (r :: BackendType -> Type) v
  = AnnUpd
  { uqp1Table   :: !(TableName b)
  , uqp1OpExps  :: ![(Column b, UpdOpExpG v)]
  , uqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , uqp1Check   :: !(AnnBoolExp b v)
  -- we don't prepare the arguments for returning
  -- however the session variable can still be
  -- converted as desired
  , uqp1Output  :: !(MutationOutputG b r v)
  , uqp1AllCols :: ![ColumnInfo b]
  } deriving (Functor, Foldable, Traversable)

type AnnUpd b = AnnUpdG b (Const Void) (SQLExpression b)

data UpdOpExpG v
  = UpdSet !v
  | UpdInc !v
  | UpdAppend !v
  | UpdPrepend !v
  | UpdDeleteKey !v
  | UpdDeleteElem !v
  | UpdDeleteAtPath ![v]
  deriving (Functor, Foldable, Traversable, Generic, Data)


-- NOTE: This function can be improved, because we use
-- the literal values defined below in the 'updateOperators'
-- function in 'Hasura.GraphQL.Schema.Mutation'. It would
-- be nice if we could avoid duplicating the string literal
-- values
updateOperatorText :: UpdOpExpG a -> Text
updateOperatorText (UpdSet          _) = "_set"
updateOperatorText (UpdInc          _) = "_inc"
updateOperatorText (UpdAppend       _) = "_append"
updateOperatorText (UpdPrepend      _) = "_prepend"
updateOperatorText (UpdDeleteKey    _) = "_delete_key"
updateOperatorText (UpdDeleteElem   _) = "_delete_elem"
updateOperatorText (UpdDeleteAtPath _) = "_delete_at_path"
