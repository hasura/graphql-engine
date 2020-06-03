module Hasura.GraphQL.Execute.Mutation where

import           Hasura.Prelude

import qualified Data.IntMap                    as IntMap
import qualified Data.Sequence                  as Seq
import qualified Database.PG.Query              as Q

import qualified Hasura.RQL.DML.Delete          as RQL
import qualified Hasura.RQL.DML.Update          as RQL

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Parser.Column
import           Hasura.RQL.Types


convertDelete
  :: RQL.AnnDelG UnpreparedValue
  -> Bool
  -> Q.TxE QErr EncJSON
convertDelete deleteOperation stringifyNum =
  RQL.deleteQueryToTx stringifyNum (preparedDelete, planVariablesSequence planningState)
  where (preparedDelete, planningState) = runIdentity $ runPlan $ RQL.traverseAnnDel prepareWithPlan deleteOperation

convertUpdate
  :: RQL.AnnUpdG UnpreparedValue
  -> Bool
  -> RespTx
convertUpdate updateOperation stringifyNum =
  -- FIXME: return empty mutation response if nothing to be inserted
  RQL.updateQueryToTx stringifyNum (preparedUpdate, planVariablesSequence planningState)
  where (preparedUpdate, planningState) = runIdentity $ runPlan $ RQL.traverseAnnUpd prepareWithPlan updateOperation


planVariablesSequence :: PlanningSt -> Seq.Seq Q.PrepArg
planVariablesSequence = Seq.fromList . map fst . IntMap.elems . _psPrepped
