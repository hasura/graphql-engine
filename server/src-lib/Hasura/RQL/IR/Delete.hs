module Hasura.RQL.IR.Delete where

import           Hasura.Prelude

import           Hasura.RQL.IR.BoolExp
import           Hasura.RQL.IR.Returning
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data AnnDelG (b :: BackendType) v
  = AnnDel
  { dqp1Table   :: !(TableName b)
  , dqp1Where   :: !(AnnBoolExp b v, AnnBoolExp b v)
  , dqp1Output  :: !(MutationOutputG b v)
  , dqp1AllCols :: ![ColumnInfo b]
  }

type AnnDel b = AnnDelG b (SQLExpression b)

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
