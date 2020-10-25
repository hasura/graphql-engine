module Hasura.Backends.Postgres.Translate.Delete
  ( mkDeleteCTE
  ) where

import           Hasura.Prelude

import qualified Data.Environment                             as Env
import qualified Data.Sequence                                as DS
import qualified Database.PG.Query                            as Q

import           Data.Aeson
import           Instances.TH.Lift                            ()

import qualified Hasura.Backends.Postgres.SQL.DML             as S
import qualified Hasura.Tracing                               as Tracing

import           Hasura.Backends.Postgres.Connection
import           Hasura.Backends.Postgres.Translate.Mutation
import           Hasura.Backends.Postgres.Translate.Returning
import           Hasura.Backends.Postgres.Translate.Select
import           Hasura.EncJSON
import           Hasura.RQL.DML.Delete.Types
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Version                        (HasVersion)


mkDeleteCTE
  :: AnnDel 'Postgres -> S.CTE
mkDeleteCTE (AnnDel tn (fltr, wc) _ _) =
  S.CTEDelete delete
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc
