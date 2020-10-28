module Hasura.Backends.Postgres.Translate.Delete
  ( mkDeleteCTE
  ) where

import           Hasura.Prelude

import           Instances.TH.Lift                          ()

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.RQL.IR.Delete
import           Hasura.RQL.Types


mkDeleteCTE
  :: AnnDel 'Postgres -> S.CTE
mkDeleteCTE (AnnDel tn (fltr, wc) _ _) =
  S.CTEDelete delete
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc
