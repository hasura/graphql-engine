module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.Environment                       as Env
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Action
import           Hasura.GraphQL.Validate.Types          (evalReusabilityT, runReusabilityT)
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.Server.Version                  (HasVersion)
import           Hasura.Session
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.LiveQuery       as E
import qualified Hasura.GraphQL.Resolve                 as RS
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.GraphQL.Validate.SelectionSet   as GV
import qualified Hasura.SQL.DML                         as S
import qualified Hasura.Tracing                         as Tracing

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

type Explain r m =
  (ReaderT r (ExceptT QErr m))

runExplain
  :: (MonadError QErr m)
  => r -> Explain r m a -> m a
runExplain ctx m =
  either throwError return =<< runExceptT (runReaderT m ctx)

resolveVal
  :: (MonadError QErr m)
  => UserInfo -> RS.UnresolvedVal -> m S.SQLExp
resolveVal userInfo = \case
  RS.UVPG annPGVal ->
    RS.txtConverter annPGVal
  RS.UVSessVar ty sessVar -> do
    sessVarVal <- S.SELit <$> getSessVarVal userInfo sessVar
    return $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PGTypeScalar colTy -> withConstructorFn colTy sessVarVal
      PGTypeArray _      -> sessVarVal
  RS.UVSQL sqlExp -> return sqlExp
  RS.UVSession -> pure $ sessionInfoJsonExp $ _uiSession userInfo

getSessVarVal
  :: (MonadError QErr m)
  => UserInfo -> SessionVariable -> m Text
getSessVarVal userInfo sessVar =
  onNothing (getSessionVariableValue sessVar sessionVariables) $
    throw400 UnexpectedPayload $
    "missing required session variable for role " <> rn <<>
    " : " <> sessionVariableToText sessVar
  where
    rn = _uiRole userInfo
    sessionVariables = _uiSession userInfo

explainField
  :: (MonadError QErr m, MonadTx m, HasVersion, MonadIO m, Tracing.MonadTrace m)
  => Env.Environment
  -> UserInfo
  -> GCtx
  -> SQLGenCtx
  -> QueryActionExecuter
  -> GV.Field
  -> m FieldPlan
explainField env userInfo gCtx sqlGenCtx actionExecuter fld =
  case fName of
    "__type"     -> return $ FieldPlan fName Nothing Nothing
    "__schema"   -> return $ FieldPlan fName Nothing Nothing
    "__typename" -> return $ FieldPlan fName Nothing Nothing
    _            -> do
      unresolvedAST <-
        runExplain (queryCtxMap, userInfo, fldMap, orderByCtx, sqlGenCtx) $
          evalReusabilityT $ RS.queryFldToPGAST env fld actionExecuter
      resolvedAST <- RS.traverseQueryRootFldAST (resolveVal userInfo) unresolvedAST
      let (query, remoteJoins) = RS.toPGQuery resolvedAST
          txtSQL = Q.getQueryText query
          -- CAREFUL!: an `EXPLAIN ANALYZE` here would actually *execute* this
          -- query, resulting in potential privilege escalation:
          withExplain = "EXPLAIN (FORMAT TEXT) " <> txtSQL
      -- Reject if query contains any remote joins
      when (remoteJoins /= mempty) $ throw400 NotSupported "Remote relationships are not allowed in explain query"
      planLines <- liftTx $ map runIdentity <$>
                     Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
      return $ FieldPlan fName (Just txtSQL) $ Just planLines
  where
    fName = GV._fName fld

    queryCtxMap = _gQueryCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx

explainGQLQuery
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> PGExecCtx
  -> (tx EncJSON -> m EncJSON)
  -> SchemaCache
  -> SQLGenCtx
  -> QueryActionExecuter
  -> GQLExplain
  -> m EncJSON
explainGQLQuery env pgExecCtx runInTx sc sqlGenCtx actionExecuter (GQLExplain query userVarsRaw maybeIsRelay) = do
  -- NOTE!: we will be executing what follows as though admin role. See e.g.
  -- notes in explainField:
  userInfo <- mkUserInfo (URBFromSessionVariablesFallback adminRoleName) UAdminSecretSent sessionVariables
  -- we don't need to check in allow list as we consider it an admin endpoint
  (execPlan, queryReusability) <- runReusabilityT $
    E.getExecPlanPartial userInfo sc queryType query
  (gCtx, rootSelSet) <- case execPlan of
    E.GExPHasura (gCtx, rootSelSet) ->
      return (gCtx, rootSelSet)
    E.GExPRemote{}  ->
      throw400 InvalidParams "only hasura queries can be explained"
  case rootSelSet of
    GV.RQuery selSet ->
      runInTx $ encJFromJValue . map snd <$>
        GV.traverseObjectSelectionSet selSet (explainField env userInfo gCtx sqlGenCtx actionExecuter)
    GV.RMutation _ ->
      throw400 InvalidParams "only queries can be explained"
    GV.RSubscription fields -> do
      (plan, _) <- E.getSubsOp env pgExecCtx gCtx sqlGenCtx userInfo
                     queryReusability actionExecuter fields
      runInTx $ encJFromJValue <$> E.explainLiveQueryPlan plan
  where
    queryType = bool E.QueryHasura E.QueryRelay $ fromMaybe False maybeIsRelay
    sessionVariables = mkSessionVariablesText $ maybe [] Map.toList userVarsRaw
