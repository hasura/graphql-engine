module Hasura.GraphQL.Execute.Mutation
  ( convertMutationSelectionSet
  , runExecutionPlan
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                         as JO
import qualified Data.Environment                           as Env
import qualified Data.HashMap.Strict                        as Map
import qualified Data.HashMap.Strict.InsOrd                 as OMap
import qualified Language.GraphQL.Draft.Syntax              as G
import qualified Network.HTTP.Client                        as HTTP
import qualified Network.HTTP.Types                         as HTTP

import           Control.Monad.Trans.Control                (MonadBaseControl)

import qualified Hasura.GraphQL.Execute.Backend             as EB
import qualified Hasura.GraphQL.Execute.RemoteJoin          as RJ
import qualified Hasura.GraphQL.Execute.Remote              as ER
import qualified Hasura.GraphQL.Transport.HTTP.Protocol     as GH
import qualified Hasura.Logging                             as L
import qualified Hasura.SQL.AnyBackend                      as AB

import           Hasura.Backends.Postgres.Instances.Execute (runPGMutationTransaction)
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Instances           ()
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Directives
import           Hasura.Metadata.Class
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Server.Version                      (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                             (MonadTrace, trace)


convertMutationSelectionSet
  :: forall m
   . (MonadError QErr m)
  => GQLContext
  -> SQLGenCtx
  -> UserInfo
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m (MutationExecutionPlan, G.SelectionSet G.NoFragments Variable)
convertMutationSelectionSet gqlContext sqlGenCtx userInfo directives fields varDefs varValsM = do
  mutationParser <- onNothing (gqlMutationParser gqlContext) $
    throw400 ValidationFailed "no mutations exist"

  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe Map.empty varValsM) directives fields
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability)
    :: (OMap.InsOrdHashMap G.Name (MutationRootField UnpreparedValue), QueryReusability)
    <-(mutationParser >>> (`onLeft` reportParseErrors)) resolvedSelSet

  -- Process directives on the mutation
  (_dirMap, _) <-  (`onLeft` reportParseErrors) =<<
    runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLMUTATION) resolvedDirectives)

  fieldPlans <- for unpreparedQueries \case
    RFDB exists ->
      AB.dispatchAnyBackend @BackendExecute exists
        \(DBField sourceName sourceConfig (MDBR db)) -> do
          let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsMutationDB db
          pure $ RFDB
                ( AB.mkAnyBackend $ DBField sourceName sourceConfig $ MDBR noRelsDBAST
                , remoteJoins
                , sqlGenCtx
                )
    RFRemote remoteField -> do
      RemoteFieldG remoteSchemaInfo resolvedRemoteField <-
        runVariableCache $ resolveRemoteField userInfo remoteField
      pure $ RFRemote $ buildExecStepRemote remoteSchemaInfo G.OperationTypeMutation $
        [G.SelectionField resolvedRemoteField]
    RFAction action -> do
      let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionMutation action
      pure $ RFAction (noRelsDBAST, remoteJoins)
    RFRaw r -> pure $ RFRaw r
  return (fieldPlans, resolvedSelSet)

coalescePostgresMutations
  :: EB.MutationExecutionPlan
  -> Maybe ( SourceConfig ('Postgres 'Vanilla)
           , InsOrdHashMap G.Name (MutationDBRoot UnpreparedValue ('Postgres 'Vanilla))
           )
coalescePostgresMutations plan = do
  -- we extract the name and config of the first mutation root, if any
  (oneSourceName, oneSourceConfig) <- case toList plan of
    RFDB (exists, _remoteJoins, _sqlGenCtx):_ ->
      AB.unpackAnyBackend @('Postgres 'Vanilla) exists <&>
      \(DBField sourceName sourceConfig (MDBR _db)) -> (sourceName, sourceConfig)
    _                                               -> Nothing
  -- we then test whether all mutations are going to that same first source
  -- and that it is Postgres and no remote joins are allowed
  mutations <- for plan \case
    RFDB (exists, remoteJoins, _sqlGenCtx) -> do
      (DBField sourceName _sourceConfig db) <- AB.unpackAnyBackend @('Postgres 'Vanilla) exists
      guard $ oneSourceName == sourceName && isNothing remoteJoins
      Just db
    _ -> Nothing
  Just (oneSourceConfig, mutations)

runExecutionPlan
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     , MonadTrace m
     , MonadQueryLog m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadMetadataStorage (MetadataStorageT m)
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> RequestId
  -> Bool
  -> MutationExecutionPlan
  -> m (EncJSON, HTTP.ResponseHeaders)
runExecutionPlan env logger userInfo httpManager reqHeaders requestId stringifyNum plan = do

  case coalescePostgresMutations plan of
    Just (sourceConfig, pgMutations) -> trace "pg_transaction" $
      (,[]) <$> runPGMutationTransaction requestId logger
        userInfo sourceConfig stringifyNum pgMutations
    Nothing -> do
      fieldResults <- flip OMap.traverseWithKey plan $
        \fieldName fieldPlan -> trace (G.unName fieldName) $ case fieldPlan of
        RFDB (exists, remoteJoins, _) -> do
          resp <- AB.dispatchAnyBackend @EB.BackendExecute exists
              \(DBField sourceName sourceConfig (MDBR db)) ->
              EB.executeMutationField requestId logger userInfo
              stringifyNum sourceName sourceConfig db
          (,[]) <$> withRemoteJoins remoteJoins resp

        RFRemote (rsi, gqlReq) -> runRemoteGQ fieldName rsi gqlReq

        RFAction (noRelsAST, remoteJoins) -> do
          let usrVars = _uiSession userInfo
          (resp, headers) <- case noRelsAST of
            AMSync s -> runQueryActionSync env logger userInfo s $
              ActionExecContext httpManager reqHeaders usrVars
            AMAsync s -> do
              actionId <- liftEitherM $ runMetadataStorageT $
                resolveActionMutationAsync s reqHeaders usrVars
              pure $ (,mempty) $ encJFromJValue $ actionIdToText actionId
          (,headers) <$> withRemoteJoins remoteJoins resp
        RFRaw value -> do
          -- introspection queries are not allowed in mutations, so we don't need
          -- to check whether or not introspection is allowed, like we do in case
          -- of a query
          pure (JO.toEncJSON value, [])

      let selectionSetResponse = encJFromInsOrdHashMap $ fst <$>
                                 OMap.mapKeys G.unName fieldResults
          headers = concatMap snd $ OMap.elems fieldResults

      pure (selectionSetResponse, headers)
  where
    withRemoteJoins remoteJoins rootResponse =
      maybe (pure rootResponse)
      (RJ.processRemoteJoins env httpManager reqHeaders userInfo $ encJToLBS rootResponse)
      remoteJoins

    runRemoteGQ fieldName rsi gqlReq = do
      (_telemTimeIO_DT, remoteResponseHeaders, resp) <-
        ER.execRemoteGQ env httpManager userInfo reqHeaders rsi gqlReq
      value <- ER.extractFieldFromResponse (G.unName fieldName) resp
      let filteredHeaders = filter ((== "Set-Cookie") . fst) remoteResponseHeaders
      pure (JO.toEncJSON value, filteredHeaders)

