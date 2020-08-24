module Hasura.RQL.DML.Delete.Types where


import           Hasura.Prelude

import qualified Hasura.SQL.DML                 as S

import           Hasura.RQL.DML.Returning.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Types

data AnnDelG v
  = AnnDel
  { dqp1Table   :: !QualifiedTable
  , dqp1Where   :: !(AnnBoolExp v, AnnBoolExp v)
  , dqp1Output  :: !(MutationOutputG v)
  , dqp1AllCols :: ![PGColumnInfo]
  } deriving (Show, Eq)

type AnnDel = AnnDelG S.SQLExp
