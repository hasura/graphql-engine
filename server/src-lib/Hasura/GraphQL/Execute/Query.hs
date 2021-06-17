module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , parseGraphQLQuery
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                as J
import qualified Data.Environment                          as Env
import qualified Data.HashMap.Strict                       as Map
import qualified Data.HashMap.Strict.InsOrd                as OMap
import qualified Language.GraphQL.Draft.Syntax             as G
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP

import qualified Hasura.GraphQL.Execute.RemoteJoin.Collect as RJ
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH
import qualified Hasura.Logging                            as L
import qualified Hasura.SQL.AnyBackend                     as AB

import           Hasura.Base.Error
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Instances          ()
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Directives
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Version                     (HasVersion)
import           Hasura.Session


parseGraphQLQuery
  :: MonadError QErr m
  => GQLContext
  -> [G.VariableDefinition]
  -> Maybe (HashMap G.Name J.Value)
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> m ( InsOrdHashMap G.Name (QueryRootField UnpreparedValue UnpreparedValue)
       , QueryReusability
       , [G.Directive Variable]
       , G.SelectionSet G.NoFragments Variable
       )
parseGraphQLQuery gqlContext varDefs varValsM directives fields = do
  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe Map.empty varValsM) directives fields
  (parsedQuery, queryReusability) <- (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors)) resolvedSelSet
  pure (parsedQuery, queryReusability, resolvedDirectives, resolvedSelSet)


convertQuerySelSet
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     , MonadGQLExecutionCheck m
     )
  => Env.Environment
  -> L.Logger L.Hasura
  -> GQLContext
  -> UserInfo
  -> HTTP.Manager
  -> HTTP.RequestHeaders
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> SetGraphqlIntrospectionOptions
  -> m (ExecutionPlan, [QueryRootField UnpreparedValue UnpreparedValue], G.SelectionSet G.NoFragments Variable, DirectiveMap)
convertQuerySelSet env logger gqlContext userInfo manager reqHeaders directives fields varDefs varValsM
  introspectionDisabledRoles = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability, normalizedDirectives, normalizedSelectionSet) <-
    parseGraphQLQuery gqlContext varDefs varValsM directives fields

  -- Transform the query plans into an execution plan
  let usrVars = _uiSession userInfo

  -- Process directives on the query
  (dirMap, _) <- (`onLeft` reportParseErrors) =<<
    runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLQUERY) normalizedDirectives)

  executionPlan <- for unpreparedQueries \case
    RFDB sourceName exists ->
      AB.dispatchAnyBackend @BackendExecute exists
        \(SourceConfigWith sourceConfig (QDBR db :: QueryDBRoot UnpreparedValue UnpreparedValue b)) -> do
          let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoins db
          dbStepInfo <- mkDBQueryPlan @b userInfo sourceName sourceConfig noRelsDBAST
          pure $ ExecStepDB [] (AB.mkAnyBackend dbStepInfo) remoteJoins
    RFRemote rf -> do
      RemoteFieldG remoteSchemaInfo remoteField <- runVariableCache $ for rf $ resolveRemoteVariable userInfo
      pure $ buildExecStepRemote remoteSchemaInfo G.OperationTypeQuery [G.SelectionField remoteField]
    RFAction action -> do
      let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
      (actionExecution, actionName, fch) <- pure $ case noRelsDBAST of
        AQQuery s -> (AEPSync $ resolveActionExecution env logger userInfo s (ActionExecContext manager reqHeaders usrVars), _aaeName s, _aaeForwardClientHeaders s)
        AQAsync s -> (AEPAsyncQuery $ AsyncActionQueryExecutionPlan (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s, _aaaqName s, _aaaqForwardClientHeaders s)
      pure $ ExecStepAction actionExecution (ActionsInfo actionName fch) remoteJoins
    RFRaw r -> flip onLeft throwError =<< executeIntrospection userInfo r introspectionDisabledRoles

  -- See Note [Temporarily disabling query plan caching]
  pure (executionPlan, OMap.elems unpreparedQueries, normalizedSelectionSet, dirMap)
