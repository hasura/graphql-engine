module Hasura.GraphQL.Execute.Mutation where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Data.HashSet                           as Set
import qualified Data.IntMap                            as IntMap
import qualified Data.Sequence                          as Seq
import qualified Data.Sequence.NonEmpty                 as NE
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.RQL.DML.Delete                  as RQL
import qualified Hasura.RQL.DML.Mutation                as RQL
import qualified Hasura.RQL.DML.Returning.Types         as RQL
import qualified Hasura.RQL.DML.Update                  as RQL
import qualified Hasura.Tracing                         as Tracing
import qualified Hasura.Logging                         as L


import           Hasura.Db
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Insert
import           Hasura.GraphQL.Execute.Prepare
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Schema.Insert
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session

convertDelete
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx)
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> RQL.AnnDelG UnpreparedValue
  -> Bool
  -> m (tx EncJSON)
convertDelete env usrVars rjCtx deleteOperation stringifyNum = do
  let (preparedDelete, expectedVariables) = flip runState Set.empty $ RQL.traverseAnnDel prepareWithoutPlan deleteOperation
  validateSessionVariables expectedVariables usrVars
  pure $ RQL.execDeleteQuery env stringifyNum (Just rjCtx) (preparedDelete, Seq.empty)

convertUpdate
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx
     )
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> RQL.AnnUpdG UnpreparedValue
  -> Bool
  -> m (tx EncJSON)
convertUpdate env usrVars rjCtx updateOperation stringifyNum = do
  let (preparedUpdate, expectedVariables) = flip runState Set.empty $ RQL.traverseAnnUpd prepareWithoutPlan updateOperation
  if null $ RQL.uqp1OpExps updateOperation
  then pure $ pure $ RQL.buildEmptyMutResp $ RQL.uqp1Output preparedUpdate
  else do
    validateSessionVariables expectedVariables usrVars
    pure $ RQL.execUpdateQuery env stringifyNum (Just rjCtx) (preparedUpdate, Seq.empty)

convertInsert
  :: ( HasVersion
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx)
  => Env.Environment
  -> SessionVariables
  -> RQL.MutationRemoteJoinCtx
  -> AnnInsert UnpreparedValue
  -> Bool
  -> m (tx EncJSON)
convertInsert env usrVars rjCtx insertOperation stringifyNum = do
  let (preparedInsert, expectedVariables) = flip runState Set.empty $ traverseAnnInsert prepareWithoutPlan insertOperation
  validateSessionVariables expectedVariables usrVars
  pure $ convertToSQLTransaction env preparedInsert rjCtx Seq.empty stringifyNum

planVariablesSequence :: SessionVariables -> PlanningSt -> Seq.Seq Q.PrepArg
planVariablesSequence usrVars = Seq.fromList . map fst . withUserVars usrVars . IntMap.elems . _psPrepped

convertMutationRootField
  :: forall m tx
  . ( HasVersion
    , MonadIO m
    , MonadError QErr m
    , Tracing.MonadTrace m
    , Tracing.MonadTrace tx
    , MonadIO tx
    , MonadTx tx
    )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> Bool
  -> MutationRootField UnpreparedValue
  -> m (Either (tx EncJSON, HTTP.ResponseHeaders) RemoteField)
convertMutationRootField env logger userInfo manager reqHeaders stringifyNum = \case
  RFDB (MDBInsert s)  -> noResponseHeaders =<< convertInsert env userSession rjCtx s stringifyNum
  RFDB (MDBUpdate s)  -> noResponseHeaders =<< convertUpdate env userSession rjCtx s stringifyNum
  RFDB (MDBDelete s)  -> noResponseHeaders =<< convertDelete env userSession rjCtx s stringifyNum
  RFRemote remote     -> pure $ Right remote
  RFAction (AMSync s) -> Left . (_aerTransaction &&& _aerHeaders) <$> resolveActionExecution env logger userInfo s actionExecContext
  RFAction (AMAsync s) -> noResponseHeaders =<< resolveActionMutationAsync s reqHeaders userSession
  RFRaw s              -> noResponseHeaders $ pure $ encJFromJValue s
  where
    noResponseHeaders :: tx EncJSON -> m (Either (tx EncJSON, HTTP.ResponseHeaders) RemoteField)
    noResponseHeaders rTx = pure $ Left (rTx, [])

    userSession = _uiSession userInfo
    actionExecContext = ActionExecContext manager reqHeaders $ _uiSession userInfo

    rjCtx = (manager, reqHeaders, userInfo)

convertMutationSelectionSet
  :: forall m tx
   . ( HasVersion
     , Tracing.MonadTrace m
     , MonadIO m
     , MonadError QErr m
     , MonadTx tx
     , Tracing.MonadTrace tx
     , MonadIO tx
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> GQLContext
  -> SQLGenCtx
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m (ExecutionPlan (tx EncJSON, HTTP.ResponseHeaders) RemoteCall (G.Name, J.Value))
convertMutationSelectionSet env logger gqlContext sqlGenCtx userInfo manager reqHeaders fields varDefs varValsM = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue), QueryReusability)
    <-  resolveVariables varDefs (fromMaybe Map.empty varValsM) fields
    >>= (mutationParser >>> (`onLeft` reportParseErrors))

  -- Transform the RQL AST into a prepared SQL query
  txs <- for unpreparedQueries $ convertMutationRootField env logger userInfo manager reqHeaders (stringifyNum sqlGenCtx)
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
      ParseError{ pePath, peMessage, peCode } ->
        throwError (err400 peCode peMessage){ qePath = pePath }

    -- | A list of aliased transactions for eg
    --
    -- > [("f1", Tx r1), ("f2", Tx r2)]
    --
    -- are converted into a single transaction as follows
    --
    -- > Tx {"f1": r1, "f2": r2}
    toSingleTx :: [(Text, tx EncJSON)] -> tx EncJSON
    toSingleTx aliasedTxs =
      fmap encJFromAssocList $
      forM aliasedTxs $ \(al, tx) -> (,) al <$> tx
    takeTx
      :: (G.Name, Either (tx EncJSON, HTTP.ResponseHeaders) RemoteField)
      -> Maybe (G.Name, (tx EncJSON, HTTP.ResponseHeaders))
    takeTx (name, Left tx) = Just (name, tx)
    takeTx _               = Nothing
    takeRemote
      :: (G.Name, Either (tx EncJSON, HTTP.ResponseHeaders) RemoteField)
      -> Maybe (G.Name, RemoteField)
    takeRemote (name, Right remote) = Just (name, remote)
    takeRemote _                    = Nothing
