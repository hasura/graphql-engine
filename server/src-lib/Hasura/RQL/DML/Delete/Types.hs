module Hasura.RQL.DML.Delete.Types where


import qualified Hasura.SQL.DML                 as S

import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend
import           Hasura.SQL.Types

data AnnDelG (b :: Backend) v
  = AnnDel
  { dqp1Table   :: !QualifiedTable
  , dqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , dqp1Output  :: !(MutationOutputG b v)
  , dqp1AllCols :: ![ColumnInfo b]
  }

type AnnDel b = AnnDelG b S.SQLExp
