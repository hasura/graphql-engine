module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import qualified Data.Aeson                                   as J
import qualified Data.Aeson.Casing                            as J
import qualified Data.Aeson.TH                                as J
import qualified Data.HashMap.Strict                          as Map
import qualified Database.PG.Query                            as Q
import qualified Language.GraphQL.Draft.Syntax                as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.GraphQL.Execute                       as E
import qualified Hasura.GraphQL.Execute.LiveQuery             as EL
import qualified Hasura.GraphQL.Execute.LiveQuery.Multiplexed as ELM
import qualified Hasura.GraphQL.Resolve                       as RS
import qualified Hasura.GraphQL.Transport.HTTP.Protocol       as GH
import qualified Hasura.GraphQL.Validate                      as GV
import qualified Hasura.SQL.DML                               as S

data GQLExplain
  = GQLExplain
  { _gqeQuery :: !GH.GQLReqParsed
  , _gqeUser  :: !(Maybe (Map.HashMap Text Text))
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.camelCase){J.omitNothingFields=True}
  ''GQLExplain
 )

data QueryFieldPlan
  = QueryFieldPlan
  { _qfpField :: !G.Name
  , _qfpSql   :: !(Maybe Text)
  , _qfpPlan  :: !(Maybe [Text])
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''QueryFieldPlan)

type Explain r =
  (ReaderT r (Except QErr))

runExplain
  :: (MonadError QErr m)
  => r -> Explain r a -> m a
runExplain ctx m =
  either throwError return $ runExcept $ runReaderT m ctx

resolveVal
  :: (MonadError QErr m)
  => UserInfo -> UnresolvedVal -> m S.SQLExp
resolveVal userInfo = \case
  RS.UVPG annPGVal ->
    txtConverter annPGVal
  RS.UVSessVar ty sessVar -> do
    sessVarVal <- S.SELit <$> getSessVarVal userInfo sessVar
    return $ flip S.SETyAnn (S.mkTypeAnn ty) $ case ty of
      PgTypeSimple colTy -> withGeoVal colTy sessVarVal
      PgTypeArray _      -> sessVarVal
  RS.UVSQL sqlExp -> return sqlExp

getSessVarVal
  :: (MonadError QErr m)
  => UserInfo -> SessVar -> m SessVarVal
getSessVarVal userInfo sessVar =
  onNothing (getVarVal sessVar usrVars) $
    throw400 UnexpectedPayload $
    "missing required session variable for role " <> rn <<>
    " : " <> sessVar
  where
    rn = userRole userInfo
    usrVars = userVars userInfo

getUnresolvedAST
  :: (MonadError QErr m)
  => UserInfo -> GCtx -> SQLGenCtx
  -> Field -> m RS.QueryRootFldUnresolved
getUnresolvedAST userInfo gCtx sqlGenCtx fld =
  runExplain (opCtxMap, userInfo, fldMap, orderByCtx, sqlGenCtx) $
  RS.queryFldToPGAST fld
  where
    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx

explainHasuraField
  :: (MonadTx m)
  => UserInfo -> RS.QueryRootFldUnresolved
  -> m (Text, [Text])
explainHasuraField userInfo unresolvedAST = do
  resolvedAST <- RS.traverseQueryRootFldAST (resolveVal userInfo)
                 unresolvedAST
  explainPgQuery (RS.toPGQuery resolvedAST) ()

explainPgQuery
  :: (MonadTx m, Q.ToPrepArgs a)
  => Q.Query
  -> a
  -> m (Text, [Text])
explainPgQuery query args = do
  let txtSQL = Q.getQueryText query
      withExplain = "EXPLAIN (FORMAT TEXT) " <> txtSQL
  planLines <- liftTx $ map runIdentity <$>
    Q.listQE dmlTxErrorHandler (Q.fromText withExplain) args True
  return (txtSQL, planLines)

data SubscriptionExplainOutput
  = SubscriptionExplainOutput
  { _seoIsMultiplexable :: !Bool
  -- only exists when subscription is not multiplexed
  , _seoReason          :: !(Maybe Text)
  , _seoSql             :: !Text
  , _seoPlan            :: ![Text]
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.camelCase) ''SubscriptionExplainOutput)

explainGQLQuery
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> SchemaCache
  -> SQLGenCtx
  -> Bool
  -> GQLExplain
  -> m EncJSON
explainGQLQuery pgExecCtx sc sqlGenCtx enableAllowList
  (GQLExplain query userVarsRaw) = do

  execPlan <- E.getExecPlanPartial userInfo sc enableAllowList query
  (gCtx, rootSelSet, varDefs) <- case execPlan of
    E.GExPHasura ctx -> return ctx
    E.GExPRemote _ _ ->
      throw400 InvalidParams "only hasura queries can be explained"
  case rootSelSet of
    GV.RQuery selSet -> do
      let tx = mapM (explainQueryField gCtx) (toList selSet)
      plans <- runTx tx
      return $ encJFromJValue plans
    GV.RMutation _ ->
      throw400 InvalidParams "only queries/subscriptions can be explained"
    GV.RSubscription subsField ->
      encJFromJValue <$> explainSubscriptionField gCtx subsField varDefs
  where
    usrVars  = mkUserVars $ maybe [] Map.toList userVarsRaw
    userInfo = mkUserInfo (fromMaybe adminRole $ roleFromVars usrVars) usrVars

    runTx :: (MonadIO m, MonadError QErr m) => LazyTx QErr a -> m a
    runTx tx = liftIO (runExceptT $ runLazyTx pgExecCtx tx) >>= liftEither

    explainQueryField :: (MonadTx m) => GCtx -> Field -> m QueryFieldPlan
    explainQueryField gCtx fld =
      case fName of
        "__type"     -> return $ QueryFieldPlan fName Nothing Nothing
        "__schema"   -> return $ QueryFieldPlan fName Nothing Nothing
        "__typename" -> return $ QueryFieldPlan fName Nothing Nothing
        _            -> do
          unresolvedAST <- getUnresolvedAST userInfo gCtx sqlGenCtx fld
          (txtSQL, planLines) <- explainHasuraField userInfo unresolvedAST
          return $ QueryFieldPlan fName (Just txtSQL) $ Just planLines
      where
        fName = _fName fld

    explainSubscriptionField
      :: (MonadIO m, MonadError QErr m)
      => GCtx -> Field -> [G.VariableDefinition] -> m SubscriptionExplainOutput
    explainSubscriptionField gCtx subsField varDefs = do

      -- we only partially resolve the live query operation so that
      -- we have the necessary information to explain the underlying queries
      astUnresolved      <- getUnresolvedAST userInfo gCtx sqlGenCtx subsField
      liveQueryOpPartial <- EL.getLiveQueryOpPartial pgExecCtx varDefs astUnresolved

      (isMultiPlexed, reasonM, explainTx) <- case liveQueryOpPartial of

        -- in case of fallback, the output will be the same as that of the query
        EL.LQFallback (_, nonConfirmingVariables) -> do
          let reason = "the following variables are not non-nullable scalars: " <>
                       GV.showVars (toList nonConfirmingVariables)
          return (False, Just reason, explainHasuraField userInfo astUnresolved)

        -- in case of multiplexed, we show the multiplexed query that is
        -- used for all such subscirptions, but it is 'explain analyz'ed with
        -- only the arguments of the given subscription
        EL.LQMultiplexed (_, mxQuery, txtEncodedVars) -> do
          -- every mx subscription needs a unique id
          respId <- liftIO ELM.newRespId
          let respVars    = ELM.getRespVars usrVars txtEncodedVars
              mxQueryArgs = ELM.mkMxQueryArgs [(respId, respVars)]
              explainTx   = explainPgQuery mxQuery mxQueryArgs
          return (True, Nothing, explainTx)
      (txtSQL, planLines) <- runTx explainTx
      return $ SubscriptionExplainOutput isMultiPlexed reasonM txtSQL planLines
