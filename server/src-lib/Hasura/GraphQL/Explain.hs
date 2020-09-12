module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashMap.Strict.InsOrd             as OMap
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Parser
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.Inline          as E
import qualified Hasura.GraphQL.Execute.LiveQuery       as E
import qualified Hasura.GraphQL.Execute.Query           as E
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.RQL.DML.RemoteJoin              as RR
import qualified Hasura.RQL.DML.Select                  as DS
import qualified Hasura.SQL.DML                         as S

data GQLExplain
  = GQLExplain
  { _gqeQuery   :: !GH.GQLReqParsed
  , _gqeUser    :: !(Maybe (Map.HashMap Text Text))
  , _gqeIsRelay :: !(Maybe Bool)
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True}
  ''GQLExplain
 )

data FieldPlan
  = FieldPlan
  { _fpField :: !G.Name
  , _fpSql   :: !(Maybe Text)
  , _fpPlan  :: !(Maybe [Text])
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''FieldPlan)

resolveUnpreparedValue
  :: (MonadError QErr m)
  => UserInfo -> UnpreparedValue -> m S.SQLExp
resolveUnpreparedValue userInfo = \case
  UVParameter pgValue _ -> pure $ toTxtValue $ pcvValue pgValue
  UVLiteral sqlExp      -> pure sqlExp
  UVSession             -> pure $ sessionInfoJsonExp $ _uiSession userInfo
  UVSessionVar ty sessionVariable -> do
    let maybeSessionVariableValue =
          getSessionVariableValue sessionVariable (_uiSession userInfo)

    sessionVariableValue <- fmap S.SELit $ onNothing maybeSessionVariableValue $
      throw400 UnexpectedPayload $ "missing required session variable for role "
      <> _uiRole userInfo <<> " : " <> sessionVariableToText sessionVariable

    pure $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessionVariableValue
      PGTypeArray _      -> sessionVariableValue

-- NOTE: This function has a 'MonadTrace' constraint in master, but we don't need it
-- here. We should evaluate if we need it here.
explainQueryField
  :: (MonadError QErr m, MonadTx m)
  => UserInfo
  -> G.Name
  -> QueryRootField UnpreparedValue
  -> m FieldPlan
explainQueryField userInfo fieldName rootField = do
  resolvedRootField <- E.traverseQueryRootField (resolveUnpreparedValue userInfo) rootField
  case resolvedRootField of
    RFRemote _ -> throw400 InvalidParams "only hasura queries can be explained"
    RFAction _ -> throw400 InvalidParams "query actions cannot be explained"
    RFRaw _    -> pure $ FieldPlan fieldName Nothing Nothing
    RFDB qDB   -> do
      let (querySQL, remoteJoins) = case qDB of
            QDBSimple s      -> first (DS.selectQuerySQL DS.JASMultipleRows) $ RR.getRemoteJoins s
            QDBPrimaryKey s  -> first (DS.selectQuerySQL DS.JASSingleObject) $ RR.getRemoteJoins s
            QDBAggregation s -> first DS.selectAggregateQuerySQL $ RR.getRemoteJoinsAggregateSelect s
            QDBConnection s  -> first DS.connectionSelectQuerySQL $ RR.getRemoteJoinsConnectionSelect s
          textSQL = Q.getQueryText querySQL
          -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
          -- query, maybe resulting in privilege escalation:
          withExplain = "EXPLAIN (FORMAT TEXT) " <> textSQL
      -- Reject if query contains any remote joins
      when (remoteJoins /= mempty) $ throw400 NotSupported "Remote relationships are not allowed in explain query"
      planLines <- liftTx $ map runIdentity <$>
                   Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
      pure $ FieldPlan fieldName (Just textSQL) $ Just planLines

-- NOTE: This function has a 'MonadTrace' constraint in master, but we don't need it
-- here. We should evaluate if we need it here.
explainGQLQuery
  :: forall m
  . ( MonadError QErr m
    , MonadIO m
    )
  => PGExecCtx
  -> SchemaCache
  -> GQLExplain
  -> m EncJSON
explainGQLQuery pgExecCtx sc (GQLExplain query userVarsRaw maybeIsRelay) = do
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
      runInTx $ encJFromJValue
        <$> for (OMap.toList unpreparedQueries) (uncurry (explainQueryField userInfo))

    G.TypedOperationDefinition G.OperationTypeMutation _ _ _ _ ->
      throw400 InvalidParams "only queries can be explained"

    G.TypedOperationDefinition G.OperationTypeSubscription _ varDefs _ selSet -> do
      -- (Here the above fragment inlining is actually executed.)
      inlinedSelSet <- E.inlineSelectionSet fragments selSet
      (unpreparedQueries, _) <- E.parseGraphQLQuery graphQLContext varDefs (GH._grVariables query) inlinedSelSet
      validSubscriptionQueries <- for unpreparedQueries E.validateSubscriptionRootField
      (plan, _) <- E.buildLiveQueryPlan pgExecCtx userInfo validSubscriptionQueries
      runInTx $ encJFromJValue <$> E.explainLiveQueryPlan plan
  where
    queryType = bool E.QueryHasura E.QueryRelay $ fromMaybe False maybeIsRelay
    sessionVariables = mkSessionVariablesText $ maybe [] Map.toList userVarsRaw

    runInTx :: LazyTx QErr EncJSON -> m EncJSON
    runInTx = liftEither <=< liftIO . runExceptT . runLazyTx pgExecCtx Q.ReadOnly
