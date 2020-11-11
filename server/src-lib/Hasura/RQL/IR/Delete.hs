module Hasura.RQL.IR.Delete where

import           Hasura.Prelude

<<<<<<< HEAD
import qualified Hasura.Backends.Postgres.SQL.DML   as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.SQL.Backend


data AnnDelG (b :: Backend) v
  = AnnDel
  { dqp1Table   :: !QualifiedTable
=======
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data AnnDelG (b :: BackendType) v
  = AnnDel
  { dqp1Table   :: !(TableName b)
>>>>>>> master
  , dqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , dqp1Output  :: !(MutationOutputG b v)
  , dqp1AllCols :: ![ColumnInfo b]
  }

<<<<<<< HEAD
type AnnDel b = AnnDelG b S.SQLExp
=======
type AnnDel b = AnnDelG b (SQLExp b)
>>>>>>> master

traverseAnnDel
  :: (Applicative f)
  => (a -> f b)
  -> AnnDelG backend a
  -> f (AnnDelG backend b)
traverseAnnDel f annUpd =
  AnnDel tn
  <$> ((,) <$> traverseAnnBoolExp f whr <*> traverseAnnBoolExp f fltr)
  <*> traverseMutationOutput f mutOutput
  <*> pure allCols
  where
    AnnDel tn (whr, fltr) mutOutput allCols = annUpd
