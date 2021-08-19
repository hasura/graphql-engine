module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , parseGraphQLQuery
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                as J
import qualified Data.Environment                          as Env
import qualified Data.HashMap.Strict                       as Map
import qualified Data.HashMap.Strict.InsOrd                as OMap
import qualified Data.Tagged                               as Tagged
import qualified Language.GraphQL.Draft.Syntax             as G
import qualified Network.HTTP.Client                       as HTTP
import qualified Network.HTTP.Types                        as HTTP

import qualified Hasura.GraphQL.Execute.RemoteJoin.Collect as RJ
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH
import qualified Hasura.Logging                            as L
import qualified Hasura.RQL.Types.QueryTags                as RQL
import qualified Hasura.SQL.AnyBackend                     as AB

import           Hasura.Base.Error
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Instances          ()
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.ParameterizedQueryHash
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Directives
import           Hasura.QueryTags
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Version                     (HasVersion)
import           Hasura.Session

import           Hasura.Server.Types                       (RequestId (..))


parseGraphQLQuery
  :: MonadError QErr m
  => GQLContext
  -> [G.VariableDefinition]
  -> Maybe (HashMap G.Name J.Value)
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> m ( InsOrdHashMap G.Name (QueryRootField UnpreparedValue)
       , [G.Directive Variable]
       , G.SelectionSet G.NoFragments Variable
       )
parseGraphQLQuery gqlContext varDefs varValsM directives fields = do
  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe Map.empty varValsM) directives fields
  parsedQuery <- (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors)) resolvedSelSet
  pure (parsedQuery, resolvedDirectives, resolvedSelSet)


convertQuerySelSet
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     , MonadGQLExecutionCheck m
     , MonadQueryTags m
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
  -> RequestId
  -> Maybe G.Name
  -- ^ Graphql Operation Name
  -> RQL.QueryTagsConfig
  -> m (ExecutionPlan, [QueryRootField UnpreparedValue], DirectiveMap, ParameterizedQueryHash)
convertQuerySelSet env logger gqlContext userInfo manager reqHeaders directives fields varDefs varValsM
  introspectionDisabledRoles reqId maybeOperationName queryTagsConfig = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, normalizedDirectives, normalizedSelectionSet) <-
    parseGraphQLQuery gqlContext varDefs varValsM directives fields

  -- Transform the query plans into an execution plan
  let usrVars = _uiSession userInfo

  -- Process directives on the query
  dirMap <- (`onLeft` reportParseErrors) =<<
    runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLQUERY) normalizedDirectives)

  let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet

  executionPlan <- flip OMap.traverseWithKey unpreparedQueries $ \rootFieldName rootFieldUnpreparedValue -> do
    case rootFieldUnpreparedValue of
      RFDB sourceName exists ->
        AB.dispatchAnyBackend @BackendExecute exists
            \(SourceConfigWith (sourceConfig :: (SourceConfig b)) (QDBR db)) -> do
              let queryTagsAttributes = encodeQueryTags $  QTQuery $ QueryMetadata reqId maybeOperationName rootFieldName parameterizedQueryHash
              let qtSourceConfig = getQueryTagsSourceConfig queryTagsConfig sourceName
              let queryTagsText = QueryTagsComment $ Tagged.untag $ createQueryTags @m (Just qtSourceConfig) queryTagsAttributes
              let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoins db
              dbStepInfo <- mkDBQueryPlan @b userInfo sourceName sourceConfig noRelsDBAST queryTagsText
              pure $ ExecStepDB [] (AB.mkAnyBackend dbStepInfo) remoteJoins
      RFRemote rf -> do
        RemoteFieldG remoteSchemaInfo resultCustomizer remoteField <- runVariableCache $ for rf $ resolveRemoteVariable userInfo
        pure $ buildExecStepRemote remoteSchemaInfo resultCustomizer G.OperationTypeQuery $ getRemoteFieldSelectionSet remoteField
      RFAction action -> do
        let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
        (actionExecution, actionName, fch) <- pure $ case noRelsDBAST of
          AQQuery s -> (AEPSync $ resolveActionExecution env logger userInfo s (ActionExecContext manager reqHeaders usrVars), _aaeName s, _aaeForwardClientHeaders s)
          AQAsync s -> (AEPAsyncQuery $ AsyncActionQueryExecutionPlan (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s, _aaaqName s, _aaaqForwardClientHeaders s)
        pure $ ExecStepAction actionExecution (ActionsInfo actionName fch) remoteJoins
      RFRaw r -> flip onLeft throwError =<< executeIntrospection userInfo r introspectionDisabledRoles
  -- See Note [Temporarily disabling query plan caching]
  pure (executionPlan, OMap.elems unpreparedQueries, dirMap, parameterizedQueryHash)
