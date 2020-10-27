module Hasura.RQL.DML.Delete.Types where

import qualified Hasura.Backends.Postgres.SQL.DML   as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend

data AnnDelG (b :: Backend) v
  = AnnDel
  { dqp1Table   :: !QualifiedTable
  , dqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , dqp1Output  :: !(MutationOutputG b v)
  , dqp1AllCols :: ![ColumnInfo b]
  }

type AnnDel b = AnnDelG b S.SQLExp
