module Hasura.RQL.IR.Update where


import           Hasura.Prelude

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data AnnUpdG (b :: Backend) v
  = AnnUpd
  { uqp1Table   :: !QualifiedTable
  , uqp1OpExps  :: ![(Column b, UpdOpExpG v)]
  , uqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , uqp1Check   :: !(AnnBoolExp b v)
  -- we don't prepare the arguments for returning
  -- however the session variable can still be
  -- converted as desired
  , uqp1Output  :: !(MutationOutputG b v)
  , uqp1AllCols :: ![ColumnInfo b]
  }

type AnnUpd b = AnnUpdG b (SQLExp b)

data UpdOpExpG v = UpdSet !v
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

traverseAnnUpd
  :: (Applicative f)
  => (a -> f b)
  -> AnnUpdG backend a
  -> f (AnnUpdG backend b)
traverseAnnUpd f annUpd =
  AnnUpd tn
  <$> traverse (traverse $ traverse f) opExps
  <*> ((,) <$> traverseAnnBoolExp f whr <*> traverseAnnBoolExp f fltr)
  <*> traverseAnnBoolExp f chk
  <*> traverseMutationOutput f mutOutput
  <*> pure allCols
  where
    AnnUpd tn opExps (whr, fltr) chk mutOutput allCols = annUpd
