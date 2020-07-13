module Hasura.GraphQL.Execute.Mutation where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.IntMap                            as IntMap
import qualified Data.Sequence                          as Seq
import qualified Data.Sequence.NonEmpty                 as NE
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.RQL.DML.Delete                  as RQL
import qualified Hasura.RQL.DML.Mutation                as RQL
import qualified Hasura.RQL.DML.Update                  as RQL

import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Resolve.Action
import           Hasura.GraphQL.Schema.Insert
import           Hasura.GraphQL.Schema.Mutation         (buildEmptyMutResp, convertToSQLTransaction,
                                                         traverseAnnInsert)
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import qualified Language.GraphQL.Draft.Syntax          as G

convertDelete
  :: (HasVersion, MonadIO m)
  => SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> RQL.AnnDelG UnpreparedValue
  -> Bool
  -> m RespTx
convertDelete usrVars rjCtx deleteOperation stringifyNum = do
  pure $ RQL.execDeleteQuery stringifyNum (Just rjCtx) (preparedDelete, planVariablesSequence usrVars planningState)
  where (preparedDelete, planningState) = runIdentity $ runPlan $ RQL.traverseAnnDel prepareWithPlan deleteOperation

convertUpdate
  :: (HasVersion, MonadIO m)
  => SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> RQL.AnnUpdG UnpreparedValue
  -> Bool
  -> m RespTx
convertUpdate usrVars rjCtx updateOperation stringifyNum = do
  pure $ if null $ RQL.uqp1OpExps updateOperation
    then pure $ buildEmptyMutResp $ RQL.uqp1Output preparedUpdate
    else RQL.execUpdateQuery stringifyNum (Just rjCtx) (preparedUpdate, planVariablesSequence usrVars planningState)
  where (preparedUpdate, planningState) = runIdentity $ runPlan $ RQL.traverseAnnUpd prepareWithPlan updateOperation

convertInsert
  :: (HasVersion, MonadIO m)
  => SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> AnnMultiInsert UnpreparedValue
  -> Bool
  -> m RespTx
convertInsert usrVars rjCtx insertOperation stringifyNum = do
  pure $ convertToSQLTransaction preparedUpdate rjCtx (planVariablesSequence usrVars planningState) stringifyNum
  where (preparedUpdate, planningState) = runIdentity $ runPlan $ traverseAnnInsert prepareWithPlan insertOperation

planVariablesSequence :: SessionVariables -> PlanningSt -> Seq.Seq Q.PrepArg
planVariablesSequence usrVars = Seq.fromList . map fst . withUserVars usrVars . IntMap.elems . _psPrepped

convertMutationRootField
  :: forall m. ( HasVersion
               , MonadIO m
               , MonadError QErr m
               )
  => UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> Bool
  -> MutationRootField UnpreparedValue
  -> m (Either (LazyRespTx, HTTP.ResponseHeaders) RemoteField)
convertMutationRootField userInfo manager reqHeaders stringifyNum = \case
  RFDB (MDBInsert s)  -> noResponseHeaders =<< convertInsert userSession rjCtx s stringifyNum
  RFDB (MDBUpdate s)  -> noResponseHeaders =<< convertUpdate userSession rjCtx s stringifyNum
  RFDB (MDBDelete s)  -> noResponseHeaders =<< convertDelete userSession rjCtx s stringifyNum
  RFRemote remote     -> pure $ Right remote
  RFAction (AMSync s) -> Left <$> first liftTx <$> resolveActionExecution userInfo s actionExecContext
  RFAction (AMAsync s) -> noResponseHeaders =<< resolveActionMutationAsync s reqHeaders userSession
  RFRaw s             -> noResponseHeaders $ pure $ encJFromJValue s
  where
    noResponseHeaders :: RespTx -> m (Either (LazyRespTx, HTTP.ResponseHeaders) RemoteField)
    noResponseHeaders rTx = pure $ Left (liftTx rTx, [])

    userSession = _uiSession userInfo
    actionExecContext = ActionExecContext manager reqHeaders $ _uiSession userInfo

    rjCtx = (manager, reqHeaders, userInfo)

convertMutationSelectionSet
  :: (HasVersion, MonadIO m, MonadError QErr m)
  => GQLContext
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m (ExecutionPlan (LazyRespTx, HTTP.ResponseHeaders) RemoteCall (G.Name, J.Value))
convertMutationSelectionSet gqlContext userInfo manager reqHeaders fields varDefs varValsM = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue), QueryReusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (mutationParser >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  -- TODO pass the correct stringifyNum somewhere rather than True
  txs <- for unpreparedQueries $ convertMutationRootField userInfo manager reqHeaders True
  let txList = OMap.toList txs
  case (mapMaybe takeTx txList, mapMaybe takeRemote txList) of
    (dbPlans, []) -> do
      let allHeaders = concatMap (snd . snd) dbPlans
          combinedTx = toSingleTx $ map (G.unName *** fst) dbPlans
      pure $ ExecStepDB (combinedTx, allHeaders)
    ([], remotes@(firstRemote:_)) -> do
      let (remoteOperation, varValsM') =
            buildTypedOperation
            G.OperationTypeMutation
            varDefs
            (map (G.SelectionField . snd . snd) remotes)
            varValsM
      if all (\remote' -> fst (snd firstRemote) == fst (snd remote')) remotes
        then return $ ExecStepRemote (fst (snd firstRemote), remoteOperation, varValsM')
        else throw400 NotSupported "Mixed remote schemas are not supported"
    _ -> throw400 NotSupported "Heterogeneous execution of database and remote schemas not supported"
  -- Build and return an executable action from the generated SQL
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
    toSingleTx :: [(Text, LazyRespTx)] -> LazyRespTx
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $
      forM aliasedTxs $ \(al, tx) -> (,) al <$> tx
    takeTx
      :: (G.Name, Either (LazyRespTx, HTTP.ResponseHeaders) RemoteField)
      -> Maybe (G.Name, (LazyRespTx, HTTP.ResponseHeaders))
    takeTx (name, Left tx) = Just (name, tx)
    takeTx _               = Nothing
    takeRemote
      :: (G.Name, Either (LazyRespTx, HTTP.ResponseHeaders) RemoteField)
      -> Maybe (G.Name, RemoteField)
    takeRemote (name, Right remote) = Just (name, remote)
    takeRemote _                    = Nothing
