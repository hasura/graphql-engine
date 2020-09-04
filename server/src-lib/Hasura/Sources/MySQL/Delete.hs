module Hasura.Sources.MySQL.Delete
  ( execDeleteQuery
  ) where

import           Instances.TH.Lift             ()

import qualified Data.Environment              as Env
import qualified Data.Sequence                 as DS
import qualified Hasura.Tracing                as Tracing

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.DML.Delete.Types
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Version         (HasVersion)
import           Hasura.Sources.MySQL.Mutation

import qualified Database.PG.Query             as Q
import qualified Hasura.SQL.DML                as S


mkDeleteCTE
  :: AnnDel -> S.CTE
mkDeleteCTE (AnnDel tn (fltr, wc) _ _) =
  S.CTEDelete delete
  where
    delete = S.SQLDelete tn Nothing tableFltr $ Just S.returningStar
    tableFltr = Just $ S.WhereFrag $
                toSQLBoolExp (S.QualTable tn) $ andAnnBoolExps fltr wc

execDeleteQuery
  ::
  ( HasVersion
  , MonadTx m
  , MonadIO m
  , Tracing.MonadTrace m
  )
  => Env.Environment
  -> Bool
  -> Maybe MutationRemoteJoinCtx
  -> (AnnDel, DS.Seq Q.PrepArg)
  -> m EncJSON
execDeleteQuery env strfyNum remoteJoinCtx (u, p) =
  runMutation env $ mkMutation remoteJoinCtx (dqp1Table u) (deleteCTE, p)
                (dqp1Output u) (dqp1AllCols u) strfyNum
  where
    deleteCTE = mkDeleteCTE u
