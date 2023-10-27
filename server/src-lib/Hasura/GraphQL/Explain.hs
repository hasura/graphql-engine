module Hasura.GraphQL.Explain
  ( explainGQLQuery,
    GQLExplain,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Context qualified as C
import Hasura.GraphQL.Execute qualified as E
import Hasura.GraphQL.Execute.Action qualified as E
import Hasura.GraphQL.Execute.Backend
import Hasura.GraphQL.Execute.Instances ()
import Hasura.GraphQL.Execute.Query qualified as E
import Hasura.GraphQL.Execute.RemoteJoin.Collect qualified as RJ
import Hasura.GraphQL.Execute.Resolve qualified as ER
import Hasura.GraphQL.Namespace (RootFieldAlias)
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Transport.Backend
import Hasura.GraphQL.Transport.HTTP.Protocol (_grOperationName, _unOperationName)
import Hasura.GraphQL.Transport.HTTP.Protocol qualified as GH
import Hasura.GraphQL.Transport.Instances ()
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session (UserAdminSecret (..), UserInfo, UserRoleBuild (..), mkSessionVariablesText, mkUserInfo)
import Hasura.Tracing (MonadTrace)
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

data GQLExplain = GQLExplain
  { _gqeQuery :: !GH.GQLReqParsed,
    _gqeUser :: !(Maybe (HashMap.HashMap Text Text)),
    _gqeIsRelay :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON GQLExplain where
  parseJSON = J.genericParseJSON hasuraJSON {J.omitNothingFields = True}

instance J.ToJSON GQLExplain where
  toJSON = J.genericToJSON hasuraJSON {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding hasuraJSON {J.omitNothingFields = True}

-- NOTE: This function has a 'MonadTrace' constraint in master, but we don't need it
-- here. We should evaluate if we need it here.
explainQueryField ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadTrace m
  ) =>
  Maybe (CredentialCache AgentLicenseKey) ->
  UserInfo ->
  [HTTP.Header] ->
  Maybe G.Name ->
  RootFieldAlias ->
  QueryRootField UnpreparedValue ->
  m EncJSON
explainQueryField agentLicenseKey userInfo reqHeaders operationName fieldName rootField = do
  case rootField of
    RFRemote _ _ -> throw400 InvalidParams "only hasura queries can be explained"
    RFAction _ -> throw400 InvalidParams "query actions cannot be explained"
    RFRaw _ -> pure $ encJFromJValue $ ExplainPlan fieldName Nothing Nothing
    RFMulti _ -> pure $ encJFromJValue $ ExplainPlan fieldName Nothing Nothing
    RFDB sourceName exists -> do
      step <- AB.dispatchAnyBackend @BackendExecute
        exists
        \(SourceConfigWith sourceConfig _ (QDBR db)) -> do
          let (newDB, remoteJoins) = RJ.getRemoteJoinsQueryDB db
          unless (isNothing remoteJoins)
            $ throw400 InvalidParams "queries with remote relationships cannot be explained"
          mkDBQueryExplain fieldName userInfo sourceName sourceConfig newDB reqHeaders operationName
      AB.dispatchAnyBackend @BackendTransport step (runDBQueryExplain agentLicenseKey)

explainGQLQuery ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadMetadataStorage m,
    MonadQueryTags m,
    MonadTrace m
  ) =>
  SchemaCache ->
  Maybe (CredentialCache AgentLicenseKey) ->
  [HTTP.Header] ->
  GQLExplain ->
  m EncJSON
explainGQLQuery sc agentLicenseKey reqHeaders (GQLExplain query userVarsRaw maybeIsRelay) = do
  -- NOTE!: we will be executing what follows as though admin role. See e.g. notes in explainField:
  userInfo <-
    mkUserInfo
      (URBFromSessionVariablesFallback adminRoleName)
      UAdminSecretSent
      sessionVariables
  -- we don't need to check in allow list as we consider it an admin endpoint
  let graphQLContext = E.makeGQLContext userInfo sc queryType
  queryParts <- GH.getSingleOperation query
  case queryParts of
    G.TypedOperationDefinition G.OperationTypeQuery _ varDefs directives inlinedSelSet -> do
      (unpreparedQueries, _, _) <-
        E.parseGraphQLQuery graphQLContext varDefs (GH._grVariables query) directives inlinedSelSet
      -- TODO: validate directives here
      encJFromList
        <$> for (InsOrdHashMap.toList unpreparedQueries) (uncurry (explainQueryField agentLicenseKey userInfo reqHeaders (_unOperationName <$> _grOperationName query)))
    G.TypedOperationDefinition G.OperationTypeMutation _ _ _ _ ->
      throw400 InvalidParams "only queries can be explained"
    G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs directives inlinedSelSet -> do
      (_normalizedDirectives, normalizedSelectionSet) <-
        ER.resolveVariables
          varDefs
          (fromMaybe mempty (GH._grVariables query))
          directives
          inlinedSelSet
      subscriptionParser <- C.gqlSubscriptionParser graphQLContext `onNothing` throw400 NotFound "no subscriptions found"
      unpreparedQueries <- liftEither $ subscriptionParser normalizedSelectionSet
      let parameterizedQueryHash = calculateParameterizedQueryHash normalizedSelectionSet
      -- TODO: validate directives here
      -- query-tags are not necessary for EXPLAIN API
      -- RequestContext are not necessary for EXPLAIN API
      ((validSubscription, _), _) <- E.buildSubscriptionPlan userInfo unpreparedQueries parameterizedQueryHash reqHeaders (_unOperationName <$> _grOperationName query)
      case validSubscription of
        E.SEAsyncActionsWithNoRelationships _ -> throw400 NotSupported "async action query fields without relationships to table cannot be explained"
        E.SEOnSourceDB (E.SSLivequery actionIds liveQueryBuilder) -> do
          actionLogResponseMap <- fst <$> E.fetchActionLogResponses actionIds
          (_, E.SubscriptionQueryPlan exists) <- liftEitherM $ liftIO $ runExceptT $ fst <$> liveQueryBuilder actionLogResponseMap
          AB.dispatchAnyBackend @BackendExecute exists \(E.MultiplexedSubscriptionQueryPlan execPlan) ->
            encJFromJValue <$> mkSubscriptionExplain execPlan
        E.SEOnSourceDB (E.SSStreaming _ ((_, E.SubscriptionQueryPlan exists), _modelInfo)) -> do
          AB.dispatchAnyBackend @BackendExecute exists \(E.MultiplexedSubscriptionQueryPlan execPlan) ->
            encJFromJValue <$> mkSubscriptionExplain execPlan
  where
    queryType = bool E.QueryHasura E.QueryRelay $ Just True == maybeIsRelay
    sessionVariables = mkSessionVariablesText $ fromMaybe mempty userVarsRaw
