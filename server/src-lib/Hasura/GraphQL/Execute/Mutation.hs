module Hasura.GraphQL.Execute.Mutation where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.IntMap                            as IntMap
import qualified Data.Sequence                          as Seq
import qualified Data.Sequence.NonEmpty                 as NE
import qualified Database.PG.Query                      as Q

import qualified Hasura.RQL.DML.Delete                  as RQL
import qualified Hasura.RQL.DML.Update                  as RQL

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Column
import           Hasura.GraphQL.Schema.Insert
import           Hasura.GraphQL.Schema.Mutation         (convertToSQLTransaction, traverseAnnInsert)
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import           Hasura.RQL.Types
import qualified Language.GraphQL.Draft.Syntax          as G


convertDelete
  :: UserVars
  -> RQL.AnnDelG UnpreparedValue
  -> Bool
  -> Q.TxE QErr EncJSON
convertDelete usrVars deleteOperation stringifyNum =
  RQL.deleteQueryToTx stringifyNum (preparedDelete, planVariablesSequence usrVars planningState)
  where (preparedDelete, planningState) = runIdentity $ runPlan $ RQL.traverseAnnDel prepareWithPlan deleteOperation

convertUpdate
  :: UserVars
  -> RQL.AnnUpdG UnpreparedValue
  -> Bool
  -> RespTx
convertUpdate usrVars updateOperation stringifyNum =
  -- FIXME: return empty mutation response if nothing to be inserted
  RQL.updateQueryToTx stringifyNum (preparedUpdate, planVariablesSequence usrVars planningState)
  where (preparedUpdate, planningState) = runIdentity $ runPlan $ RQL.traverseAnnUpd prepareWithPlan updateOperation

convertInsert
  :: UserVars
  -> AnnMultiInsert UnpreparedValue
  -> Bool
  -> RespTx
convertInsert userVars insertOperation stringifyNum =
  convertToSQLTransaction preparedUpdate stringifyNum
  where (preparedUpdate, _FIXME) = runIdentity $ runPlan $ traverseAnnInsert prepareWithPlan insertOperation

planVariablesSequence :: UserVars -> PlanningSt -> Seq.Seq Q.PrepArg
planVariablesSequence usrVars = Seq.fromList . map fst . withUserVars usrVars . IntMap.elems . _psPrepped

convertMutationRootField
  :: UserVars
  -> Bool
  -> MutationRootField UnpreparedValue
  -> RespTx
convertMutationRootField usrVars stringifyNum = \case
  RFDB (MDBInsert s) -> convertInsert usrVars s stringifyNum
  RFDB (MDBUpdate s) -> convertUpdate usrVars s stringifyNum
  RFDB (MDBDelete s) -> convertDelete usrVars s stringifyNum
  RFRaw s    -> return $ encJFromJValue s

convertMutationSelectionSet
  :: MonadError QErr m
  => GQLContext
  -> UserVars
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m LazyRespTx
convertMutationSelectionSet gqlContext usrVars fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue), QueryReusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (gqlMutationParser gqlContext >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  -- TODO pass the correct stringifyNum somewhere rather than True
  let txs = convertMutationRootField usrVars True <$> unpreparedQueries

  -- Build and return an executable action from the generated SQL
  pure $ liftTx $ toSingleTx $ map (\(name, bla) -> (G.unName name, bla)) $ OMap.toList txs
  where
    reportParseErrors errs = case NE.head errs of
      -- TODO: Our error reporting machinery doesn’t currently support reporting
      -- multiple errors at once, so we’re throwing away all but the first one
      -- here. It would be nice to report all of them!
      ParseError{ pePath, peMessage } ->
        throwError (err400 ValidationFailed peMessage){ qePath = pePath }

    -- | A list of aliased transactions for eg
    --
    -- > [("f1", Tx r1), ("f2", Tx r2)]
    --
    -- are converted into a single transaction as follows
    --
    -- > Tx {"f1": r1, "f2": r2}
    toSingleTx :: [(Text, RespTx)] -> RespTx
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $
      forM aliasedTxs $ \(al, tx) -> (,) al <$> tx
