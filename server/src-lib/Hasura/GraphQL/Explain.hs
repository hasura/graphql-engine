module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                as J
import qualified Data.Aeson.Casing                         as J
import qualified Data.Aeson.TH                             as J
import qualified Data.HashMap.Strict                       as Map
import qualified Data.HashMap.Strict.InsOrd                as OMap
import qualified Database.PG.Query                         as Q
import qualified Language.GraphQL.Draft.Syntax             as G

import           Control.Monad.Trans.Control               (MonadBaseControl)
import           Data.Text.Extended
import           Data.Typeable                             (cast)

import qualified Hasura.Backends.Postgres.SQL.DML          as S
import qualified Hasura.Backends.Postgres.Translate.Select as DS
import qualified Hasura.GraphQL.Execute                    as E
import qualified Hasura.GraphQL.Execute.Backend            as E
import qualified Hasura.GraphQL.Execute.Inline             as E
import qualified Hasura.GraphQL.Execute.LiveQuery.Explain  as E
import qualified Hasura.GraphQL.Execute.LiveQuery.Plan     as EL
import qualified Hasura.GraphQL.Execute.Query              as E
import qualified Hasura.GraphQL.Execute.RemoteJoin         as RR
import qualified Hasura.GraphQL.Transport.HTTP.Protocol    as GH

import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.Column (toTxtValue)
import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.Session


data GQLExplain
  = GQLExplain
  { _gqeQuery   :: !GH.GQLReqParsed
  , _gqeUser    :: !(Maybe (Map.HashMap Text Text))
  , _gqeIsRelay :: !(Maybe Bool)
  } deriving (Show, Eq)

$(J.deriveJSON hasuraJSON{J.omitNothingFields=True}
  ''GQLExplain
 )

data FieldPlan
  = FieldPlan
  { _fpField :: !G.Name
  , _fpSql   :: !(Maybe Text)
  , _fpPlan  :: !(Maybe [Text])
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonPrefix J.camelCase) ''FieldPlan)

resolveUnpreparedValue
  :: (MonadError QErr m)
  => UserInfo -> UnpreparedValue 'Postgres -> m S.SQLExp
resolveUnpreparedValue userInfo = \case
  UVParameter _ cv      -> pure $ toTxtValue cv
  UVLiteral sqlExp      -> pure sqlExp
  UVSession             -> pure $ sessionInfoJsonExp $ _uiSession userInfo
  UVSessionVar ty sessionVariable -> do
    let maybeSessionVariableValue =
          getSessionVariableValue sessionVariable (_uiSession userInfo)

    sessionVariableValue <- fmap S.SELit $ onNothing maybeSessionVariableValue $
      throw400 UnexpectedPayload $ "missing required session variable for role "
      <> _uiRole userInfo <<> " : " <> sessionVariableToText sessionVariable

    pure $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      CollectableTypeScalar colTy -> withConstructorFn colTy sessionVariableValue
      CollectableTypeArray _      -> sessionVariableValue

-- NOTE: This function has a 'MonadTrace' constraint in master, but we don't need it
-- here. We should evaluate if we need it here.
explainQueryField
  :: ( MonadError QErr m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => UserInfo
  -> G.Name
  -> QueryRootField UnpreparedValue
  -> m (Maybe FieldPlan)
explainQueryField userInfo fieldName rootField = do
  case rootField of
    RFRemote _ -> throw400 InvalidParams "only hasura queries can be explained"
    RFAction _ -> throw400 InvalidParams "query actions cannot be explained"
    RFRaw _    -> pure $ Just $ FieldPlan fieldName Nothing Nothing
    RFDB _ config (QDBR qDB) -> runMaybeT $ do
      -- TEMPORARY!!!
      -- We don't handle non-Postgres backends yet: for now, we filter root fields to only keep those
      -- that are targeting postgres, and we *silently* discard all the others. This is fine for now, as
      -- we haven't integrated any other backend yet, but will need to be fixed as soon as possible for
      -- other backends to work.
      pgConfig <- hoistMaybe $ cast config
      pgQDB    <- hoistMaybe $ cast qDB
      lift $ do
        resolvedQuery <- E.traverseQueryDB (resolveUnpreparedValue userInfo) pgQDB
        let (querySQL, remoteJoins) = case resolvedQuery of
              QDBMultipleRows s -> first (DS.selectQuerySQL JASMultipleRows) $ RR.getRemoteJoinsSelect s
              QDBSingleRow    s -> first (DS.selectQuerySQL JASSingleObject) $ RR.getRemoteJoinsSelect s
              QDBAggregation  s -> first DS.selectAggregateQuerySQL $ RR.getRemoteJoinsAggregateSelect s
              QDBConnection   s -> first DS.connectionSelectQuerySQL $ RR.getRemoteJoinsConnectionSelect s
            textSQL = Q.getQueryText querySQL
            -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
            -- query, maybe resulting in privilege escalation:
            withExplain = "EXPLAIN (FORMAT TEXT) " <> textSQL
        -- Reject if query contains any remote joins
        when (remoteJoins /= mempty) $ throw400 NotSupported "Remote relationships are not allowed in explain query"
        planLines <- liftEitherM $ runExceptT $ runLazyTx (_pscExecCtx pgConfig) Q.ReadOnly $
                     liftTx $ map runIdentity <$>
                     Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
        pure $ FieldPlan fieldName (Just textSQL) $ Just planLines

-- NOTE: This function has a 'MonadTrace' constraint in master, but we don't need it
-- here. We should evaluate if we need it here.
explainGQLQuery
  :: forall m
  . ( MonadError QErr m
    , MonadIO m
    , MonadBaseControl IO m
    )
  => SchemaCache
  -> GQLExplain
  -> m EncJSON
explainGQLQuery sc (GQLExplain query userVarsRaw maybeIsRelay) = do
  -- NOTE!: we will be executing what follows as though admin role. See e.g. notes in explainField:
  userInfo <- mkUserInfo (URBFromSessionVariablesFallback adminRoleName) UAdminSecretSent sessionVariables
  -- we don't need to check in allow list as we consider it an admin endpoint
  let takeFragment =
        \case G.ExecutableDefinitionFragment f -> Just f; _ -> Nothing
      fragments = mapMaybe takeFragment $ GH.unGQLExecDoc $ GH._grQuery query
  (graphQLContext, queryParts) <- E.getExecPlanPartial userInfo sc queryType query
  case queryParts of
    G.TypedOperationDefinition G.OperationTypeQuery _ varDefs _ selSet -> do
      -- (Here the above fragment inlining is actually executed.)
      inlinedSelSet <- E.inlineSelectionSet fragments selSet
      (unpreparedQueries, _) <-
        E.parseGraphQLQuery graphQLContext varDefs (GH._grVariables query) inlinedSelSet
      encJFromJValue . catMaybes
        <$> for (OMap.toList unpreparedQueries) (uncurry (explainQueryField userInfo))

    G.TypedOperationDefinition G.OperationTypeMutation _ _ _ _ ->
      throw400 InvalidParams "only queries can be explained"

    G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs _ selSet -> do
      -- (Here the above fragment inlining is actually executed.)
      inlinedSelSet <- E.inlineSelectionSet fragments selSet
      (unpreparedQueries, _) <- E.parseGraphQLQuery graphQLContext varDefs (GH._grVariables query) inlinedSelSet
      (_, E.LQP (execPlan :: EL.LiveQueryPlan b (E.MultiplexedQuery b))) <- E.createSubscriptionPlan userInfo unpreparedQueries
      case backendTag @b of
        PostgresTag -> encJFromJValue <$> E.explainLiveQueryPlan execPlan
        MSSQLTag    -> pure mempty
  where
    queryType = bool E.QueryHasura E.QueryRelay $ Just True == maybeIsRelay
    sessionVariables = mkSessionVariablesText $ fromMaybe mempty userVarsRaw
