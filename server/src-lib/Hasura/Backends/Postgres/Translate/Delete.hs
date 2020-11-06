module Hasura.Backends.Postgres.Translate.Delete
  ( mkDelete
  ) where

import           Hasura.Prelude

import           Instances.TH.Lift                          ()

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.RQL.IR.Delete
import           Hasura.RQL.Types

mkDelete :: AnnDel 'Postgres -> S.SQLDelete
mkDelete (AnnDel tn (fltr, wc) _ _) =
  S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
  where
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc
