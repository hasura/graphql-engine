module Hasura.GraphQL.Execute.Query
  ( convertQuerySelSet
  , parseGraphQLQuery
  , runExecutionPlan
  , MonadExecuteQuery(..)
  , QueryCacheKey(..)
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Ordered                     as JO
import qualified Data.Dependent.Map                     as DM
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as HTTP

import           Control.Lens                           (toListOf)
import           Control.Monad.Morph                    (hoist)
import           Control.Monad.Trans.Control            (MonadBaseControl)

import qualified Hasura.GraphQL.Execute.Backend         as EB
import qualified Hasura.GraphQL.Execute.Remote          as ER
import qualified Hasura.GraphQL.Execute.RemoteJoin      as RJ
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.Logging                         as L
import qualified Hasura.SQL.AnyBackend                  as AB
import qualified Hasura.Tracing                         as Tracing

import           Hasura.Base.Error

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Execute.Action
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Execute.Common
import           Hasura.GraphQL.Execute.Instances       ()
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Parser
import           Hasura.GraphQL.Parser.Directives
import           Hasura.Metadata.Class
import           Hasura.RQL.IR
import           Hasura.RQL.Types
import           Hasura.Server.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.Tracing                         (MonadTrace, TraceT, trace)


parseGraphQLQuery
  :: MonadError QErr m
  => GQLContext
  -> [G.VariableDefinition]
  -> Maybe (HashMap G.Name J.Value)
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> m ( InsOrdHashMap G.Name (QueryRootField UnpreparedValue)
       , QueryReusability
       , [G.Directive Variable]
       , G.SelectionSet G.NoFragments Variable
       )
parseGraphQLQuery gqlContext varDefs varValsM directives fields = do
  (resolvedDirectives, resolvedSelSet) <- resolveVariables varDefs (fromMaybe Map.empty varValsM) directives fields
  (parsedQuery, queryReusability) <- (gqlQueryParser gqlContext >>> (`onLeft` reportParseErrors)) resolvedSelSet
  pure (parsedQuery, queryReusability, resolvedDirectives, resolvedSelSet)



-- | A predicate on session variables. The 'Monoid' instance makes it simple
-- to combine several predicates disjunctively.
newtype SessVarPred
  = SessVarPred { unSessVarPred :: SessionVariable -> SessionVariableValue -> Bool }

keepAllSessionVariables :: SessVarPred
keepAllSessionVariables = SessVarPred $ \_ _ -> True

instance Semigroup SessVarPred where
  SessVarPred p1 <> SessVarPred p2 = SessVarPred $ \sv svv ->
    p1 sv svv || p2 sv svv

instance Monoid SessVarPred where
  mempty = SessVarPred $ \_ _ -> False

runSessVarPred :: SessVarPred -> SessionVariables -> SessionVariables
runSessVarPred = filterSessionVariables . unSessVarPred

-- | Filter out only those session variables used by the query AST provided
filterVariablesFromQuery
  :: [QueryRootField UnpreparedValue] -> SessVarPred
filterVariablesFromQuery query = fold $ rootToSessVarPreds =<< query
  where
    rootToSessVarPreds = \case
      RFDB exists -> AB.dispatchAnyBackend @Backend exists \case
          DBField _ _ (QDBR db) -> toPred <$> toListOf traverseQueryDB db
      RFAction actionQ -> toPred <$> toListOf traverseActionQuery actionQ
      _ -> []

    toPred :: UnpreparedValue bet -> SessVarPred
    -- if we see a reference to the whole session variables object,
    -- then we need to keep everything:
    toPred UVSession               = keepAllSessionVariables
    -- if we only see a specific session variable, we only need to keep that one:
    toPred (UVSessionVar _type sv) = SessVarPred $ \sv' _ -> sv == sv'
    toPred _                       = mempty

data QueryCacheKey = QueryCacheKey
  { qckQueryString :: !GH.GQLReqParsed
  , qckUserRole    :: !RoleName
  , qckSession     :: !SessionVariables
  }

instance J.ToJSON QueryCacheKey where
  toJSON (QueryCacheKey qs ur sess) =
    J.object ["query_string" J..= qs, "user_role" J..= ur, "session" J..= sess]

class Monad m => MonadExecuteQuery m where
  -- | This method does two things: it looks up a query result in the
  -- server-side cache, if a cache is used, and it additionally returns HTTP
  -- headers that can instruct a client how long a response can be cached
  -- locally (i.e. client-side).
  cacheLookup
    :: [RemoteSchemaInfo]
    -- ^ Used to check if the elaborated query supports caching
    -> [ActionsInfo]
    -- ^ Used to check if actions query supports caching (unsupported if `forward_client_headers` is set)
    -> QueryCacheKey
    -- ^ Key that uniquely identifies the result of a query execution
    -> Maybe CachedDirective
    -- ^ Cached Directive from GraphQL query AST
    -> TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, Maybe EncJSON)
    -- ^ HTTP headers to be sent back to the caller for this GraphQL request,
    -- containing e.g. time-to-live information, and a cached value if found and
    -- within time-to-live.  So a return value (non-empty-ttl-headers, Nothing)
    -- represents that we don't have a server-side cache of the query, but that
    -- the client should store it locally.  The value ([], Just json) represents
    -- that the client should not store the response locally, but we do have a
    -- server-side cache value that can be used to avoid query execution.

  -- | Store a json response for a query that we've executed in the cache.  Note
  -- that, as part of this, 'cacheStore' has to decide whether the response is
  -- cacheable.  A very similar decision is also made in 'cacheLookup', since it
  -- has to construct corresponding cache-enabling headers that are sent to the
  -- client.  But note that the HTTP headers influence client-side caching,
  -- whereas 'cacheStore' changes the server-side cache.
  cacheStore
    :: QueryCacheKey
    -- ^ Key under which to store the result of a query execution
    -> Maybe CachedDirective
    -- ^ Cached Directive from GraphQL query AST
    -> EncJSON
    -- ^ Result of a query execution
    -> TraceT (ExceptT QErr m) ()
    -- ^ Always succeeds

  default cacheLookup :: (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    [RemoteSchemaInfo] -> [ActionsInfo] -> QueryCacheKey -> Maybe CachedDirective -> TraceT (ExceptT QErr m) (HTTP.ResponseHeaders, Maybe EncJSON)
  cacheLookup a b c d = hoist (hoist lift) $ cacheLookup a b c d

  default cacheStore :: (m ~ t n, MonadTrans t, MonadExecuteQuery n) =>
    QueryCacheKey -> Maybe CachedDirective -> EncJSON -> TraceT (ExceptT QErr m) ()
  cacheStore a b c = hoist (hoist lift) $ cacheStore  a b c

instance MonadExecuteQuery m => MonadExecuteQuery (ReaderT r m)
instance MonadExecuteQuery m => MonadExecuteQuery (ExceptT r m)
instance MonadExecuteQuery m => MonadExecuteQuery (TraceT m)
instance MonadExecuteQuery m => MonadExecuteQuery (MetadataStorageT m)

convertQuerySelSet
  :: (MonadError QErr m)
  => GQLContext
  -> UserInfo
  -> [G.Directive G.Name]
  -> G.SelectionSet G.NoFragments G.Name
  -> [G.VariableDefinition]
  -> Maybe GH.VariableValues
  -> m (QueryExecutionPlan, [QueryRootField UnpreparedValue], G.SelectionSet G.NoFragments Variable, DirectiveMap)
convertQuerySelSet gqlContext userInfo directives fields varDefs varValsM = do
  -- Parse the GraphQL query into the RQL AST
  (unpreparedQueries, _reusability, normalizedDirectives, normalizedSelectionSet) <-
    parseGraphQLQuery gqlContext varDefs varValsM directives fields

  -- Process directives on the query
  (dirMap, _) <- (`onLeft` reportParseErrors) =<<
    runParseT (parseDirectives customDirectives (G.DLExecutable G.EDLQUERY) normalizedDirectives)

  executionPlan <- for unpreparedQueries \case
    RFDB exists ->
      AB.dispatchAnyBackend @BackendExecute exists
        \(DBField sourceName sourceConfig (QDBR db)) -> do
          let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoins db
          pure $ RFDB
                ( AB.mkAnyBackend $ DBField sourceName sourceConfig $ QDBR noRelsDBAST
                , remoteJoins
                )
    RFRemote rf -> do
      RemoteFieldG remoteSchemaInfo remoteField <- runVariableCache $ for rf $ resolveRemoteVariable userInfo
      pure $ RFRemote $ buildExecStepRemote remoteSchemaInfo G.OperationTypeQuery [G.SelectionField remoteField]
    RFAction action -> do
      let (noRelsDBAST, remoteJoins) = RJ.getRemoteJoinsActionQuery action
      pure $ RFAction (noRelsDBAST, remoteJoins)
    RFRaw r -> pure $ RFRaw r

  -- See Note [Temporarily disabling query plan caching]
  pure (executionPlan, OMap.elems unpreparedQueries, normalizedSelectionSet, dirMap)

runExecutionPlan
  :: forall m .
     ( HasVersion
     , MonadError QErr m
     , MonadGQLExecutionCheck m
     , MonadTrace m
     , MonadExecuteQuery m
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
  -> SetGraphqlIntrospectionOptions
  -> GH.GQLReqParsed
  -> QueryExecutionPlan
  -> [QueryRootField UnpreparedValue]
  -> DirectiveMap
  -> m (EncJSON, HTTP.ResponseHeaders)
runExecutionPlan env logger userInfo httpManager reqHeaders requestId introspectionOptions reqParsed queryPlans asts dirMap = do
  let filteredSessionVars = runSessVarPred (filterVariablesFromQuery asts) (_uiSession userInfo)
      cacheKey = QueryCacheKey reqParsed (_uiRole userInfo) filteredSessionVars
      remoteSchemas = OMap.elems queryPlans >>= \case
        RFDB (_dbAST, remoteJoins) -> do
            maybe [] (map RJ._rsjRemoteSchema . RJ.getRemoteSchemaJoins) remoteJoins
        _ -> []
      actionsInfo = flip mapMaybe (OMap.elems queryPlans) $ \case
        RFAction (fld, _remoteJoins) ->
          case fld of
            AQQuery s -> Just $ ActionsInfo (_aaeName s) (_aaeForwardClientHeaders s)
            AQAsync s -> Just $ ActionsInfo (_aaaqName s) (_aaaqForwardClientHeaders s)
        _                                  -> Nothing
      cachedDirective_ = runIdentity <$> DM.lookup cached dirMap

  -- Fetch the response from the cache
  (responseHeaders, cachedValue) <-
    Tracing.interpTraceT (liftEitherM . runExceptT) $
      cacheLookup remoteSchemas actionsInfo cacheKey cachedDirective_

  case cachedValue of
    Just cachedResponseData ->
      pure $ (cachedResponseData, responseHeaders)
    Nothing -> do
      resp <- runExecutionPlanNoCache env logger userInfo httpManager reqHeaders requestId introspectionOptions queryPlans
      Tracing.interpTraceT (liftEitherM . runExceptT) $ cacheStore cacheKey cachedDirective_ $ fst resp
      pure resp

runExecutionPlanNoCache
  :: forall m .
     ( MonadError QErr m
     , HasVersion
     , MonadGQLExecutionCheck m
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
  -> SetGraphqlIntrospectionOptions
  -> QueryExecutionPlan
  -> m (EncJSON, HTTP.ResponseHeaders)
runExecutionPlanNoCache env logger userInfo httpManager reqHeaders requestId introspectionOptions queryPlans = do
  fieldResults <- flip OMap.traverseWithKey queryPlans $
    \fieldName fieldPlan -> trace (G.unName fieldName) $ case fieldPlan of
    RFDB (exists, remoteJoins) -> do
      resp <- AB.dispatchAnyBackend @EB.BackendExecute exists
          \(DBField sourceName sourceConfig (QDBR db)) ->
          EB.executeQueryField requestId logger userInfo sourceName sourceConfig db
      (,[]) <$> withRemoteJoins remoteJoins resp
    RFRemote (rsi, gqlReq) ->
      runRemoteGQ fieldName rsi gqlReq
    RFAction (noRelsAST, remoteJoins) -> do
      let usrVars = _uiSession userInfo
      (resp, headers) <- case noRelsAST of
        AQQuery s -> runQueryActionSync env logger userInfo s (ActionExecContext httpManager reqHeaders usrVars)
        AQAsync s -> runQueryActionAsync $ AsyncActionQueryExecutionPlan (_aaaqActionId s) $ resolveAsyncActionQuery userInfo s
      (,headers) <$> withRemoteJoins remoteJoins resp
    RFRaw value -> do
      introspectionResult <- either throwError pure =<<
        executeIntrospection userInfo value introspectionOptions
      pure (JO.toEncJSON introspectionResult, [])

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

