module Hasura.Backends.Postgres.Translate.Delete
<<<<<<< HEAD
  ( mkDeleteCTE
=======
  ( mkDelete
>>>>>>> master
  ) where

import           Hasura.Prelude

<<<<<<< HEAD
import           Instances.TH.Lift                ()

import qualified Hasura.Backends.Postgres.SQL.DML as S

import           Hasura.RQL.GBoolExp
import           Hasura.RQL.IR.Delete
import           Hasura.RQL.Types


mkDeleteCTE
  :: AnnDel 'Postgres -> S.CTE
mkDeleteCTE (AnnDel tn (fltr, wc) _ _) =
  S.CTEDelete delete
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
=======
import           Instances.TH.Lift                          ()

import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.RQL.IR.Delete
import           Hasura.RQL.Types

mkDelete :: AnnDel 'Postgres -> S.SQLDelete
mkDelete (AnnDel tn (fltr, wc) _ _) =
  S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
  where
>>>>>>> master
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc
