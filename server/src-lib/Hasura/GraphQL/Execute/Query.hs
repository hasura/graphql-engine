module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet,
    parseGraphQLQuery,
  )
where

import Data.Aeson qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Tagged qualified as Tagged
import Hasura.Base.Error
import Hasura.GraphQL.Context
import Hasura.GraphQL.Execute.Action
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Common
import Hasura.GraphQL.Execute.Instances ()
import Hasura.GraphQL.Execute.Remote
import Hasura.GraphQL.Execute.RemoteJoin.Collect qualified as RJ
import Hasura.GraphQL.Execute.Resolve
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Parser
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.GraphqlSchemaIntrospection
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types (RequestId (..))
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP

parseGraphQLQuery ::
  MonadError QErr m =>
  GQLContext ->
  [G.VariableDefinition] ->
  Maybe (HashMap G.Name J.Value) ->
  [G.Directive G.Name] ->
  G.SelectionSet G.NoFragments G.Name ->
  m
    ( RootFieldMap (QueryRootField UnpreparedValue),
      [G.Directive Variable],
      G.SelectionSet G.NoFragments Variable
    )
parseGraphQLQuery gqlContext varDefs varValsM directives fields = do
  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe Map.empty varValsM) directives fields
  parsedQuery <- (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors)) resolvedSelSet
  pure (parsedQuery, resolvedDirectives, resolvedSelSet)

-- | Construct an 'ExecutionPlan' from a 'G.SelectionSet'.
convertQuerySelSet ::
  forall m.
  ( MonadError QErr m,
    MonadGQLExecutionCheck m,
    MonadQueryTags m
  ) =>
  Env.Environment ->
  L.Logger L.Hasura ->
  GQLContext ->
  UserInfo ->
  HTTP.Manager ->
  HTTP.RequestHeaders ->
  [G.Directive G.Name] ->
  G.SelectionSet G.NoFragments G.Name ->
  [G.VariableDefinition] ->
  GH.GQLReqUnparsed ->
  SetGraphqlIntrospectionOptions ->
  RequestId ->
  -- | Graphql Operation Name
  Maybe G.Name ->
  m (ExecutionPlan, [QueryRootField UnpreparedValue], DirectiveMap, ParameterizedQueryHash)
convertQuerySelSet
  env
  logger
  gqlContext
  userInfo
  manager
  reqHeaders
  directives
  fields
  varDefs
  gqlUnparsed
  introspectionDisabledRoles
  reqId
  maybeOperationName = do
    -- 1. Parse the GraphQL query into the 'RootFieldMap' and a 'SelectionSet'
    (unpreparedQueries, normalizedDirectives, normalizedSelectionSet) <-
      parseGraphQLQuery gqlContext varDefs (GH._grVariables gqlUnparsed) directives fields

    -- 2. Parse directives on the query
    dirMap <-
      (`onLeft` reportParseErrors)
        =<< runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLQUERY) normalizedDirectives)

    let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet

    -- 3. Transform the 'RootFieldMap' into an execution plan
    executionPlan <- flip OMap.traverseWithKey unpreparedQueries $ \rootFieldName rootFieldUnpreparedValue -> do
      case rootFieldUnpreparedValue of
        RFDB sourceName exists ->
          AB.dispatchAnyBackend @BackendExecute
            exists
            \(SourceConfigWith (sourceConfig :: (SourceConfig b)) queryTagsConfig (QDBR db)) -> do
              let queryTagsAttributes = encodeQueryTags $ QTQuery $ QueryMetadata reqId maybeOperationName rootFieldName parameterizedQueryHash
                  queryTagsComment = Tagged.untag $ createQueryTags @m queryTagsAttributes queryTagsConfig
                  (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsQueryDB db
              dbStepInfo <- flip runReaderT queryTagsComment $ mkDBQueryPlan @b userInfo sourceName sourceConfig noRelsDBAST
              pure $ ExecStepDB [] (AB.mkAnyBackend dbStepInfo) remoteJoins
        RFRemote rf -> do
          RemoteSchemaRootField remoteSchemaInfo resultCustomizer remoteField <- runVariableCache $ for rf $ resolveRemoteVariable userInfo
          let (noRelsRemoteField, remoteJoins) = RJ.getRemoteJoinsGraphQLField remoteField
          pure $ buildExecStepRemote remoteSchemaInfo resultCustomizer G.OperationTypeQuery noRelsRemoteField remoteJoins (GH._grOperationName gqlUnparsed)
        RFAction action -> do
          let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
          (actionExecution, actionName, fch) <- pure $ case noRelsDBAST of
            AQQuery s -> (AEPSync $ resolveActionExecution env logger userInfo s (ActionExecContext manager reqHeaders (_uiSession userInfo)) (Just (GH._grQuery gqlUnparsed)), _aaeName s, _aaeForwardClientHeaders s)
            AQAsync s -> (AEPAsyncQuery $ AsyncActionQueryExecutionPlan (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s, _aaaqName s, _aaaqForwardClientHeaders s)
          pure $ ExecStepAction actionExecution (ActionsInfo actionName fch) remoteJoins
        RFRaw r -> flip onLeft throwError =<< executeIntrospection userInfo r introspectionDisabledRoles
    pure (executionPlan, OMap.elems unpreparedQueries, dirMap, parameterizedQueryHash)
