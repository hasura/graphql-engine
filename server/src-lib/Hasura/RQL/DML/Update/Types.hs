module Hasura.RQL.DML.Update.Types where


import           Hasura.Prelude

import qualified Hasura.SQL.DML                 as S

import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend
import           Hasura.SQL.Types


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

type AnnUpd b = AnnUpdG b S.SQLExp

data UpdOpExpG v = UpdSet !v
                 | UpdInc !v
                 | UpdAppend !v
                 | UpdPrepend !v
                 | UpdDeleteKey !v
                 | UpdDeleteElem !v
                 | UpdDeleteAtPath ![v]
                 deriving (Functor, Foldable, Traversable, Generic, Data)
