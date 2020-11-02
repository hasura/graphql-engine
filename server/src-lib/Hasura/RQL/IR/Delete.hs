module Hasura.RQL.IR.Delete where

import           Hasura.Prelude

import qualified Hasura.Backends.Postgres.SQL.DML   as S

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
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
