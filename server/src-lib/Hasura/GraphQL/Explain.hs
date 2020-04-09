module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Validate.Types          (evalReusabilityT, runReusabilityT)
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value
import           Hasura.Server.Version             (HasVersion)
import           Hasura.GraphQL.Resolve.Action

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.LiveQuery       as E
import qualified Hasura.GraphQL.Resolve                 as RS
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.SQL.DML                         as S

data GQLExplain
  = GQLExplain
  { _gqeQuery :: !GH.GQLReqParsed
  , _gqeUser  :: !(Maybe (Map.HashMap Text Text))
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.camelCase){J.omitNothingFields=True}
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
  RS.UVSession -> pure $ sessionInfoJsonExp $ userVars userInfo

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

explainField
  :: (MonadError QErr m, MonadTx m, HasVersion, MonadIO m)
  => UserInfo
  -> GCtx
  -> SQLGenCtx
  -> QueryActionExecuter
  -> GV.Field
  -> m FieldPlan
explainField userInfo gCtx sqlGenCtx actionExecuter fld =
  case fName of
    "__type"     -> return $ FieldPlan fName Nothing Nothing
    "__schema"   -> return $ FieldPlan fName Nothing Nothing
    "__typename" -> return $ FieldPlan fName Nothing Nothing
    _            -> do
      unresolvedAST <-
        runExplain (queryCtxMap, userInfo, fldMap, orderByCtx, sqlGenCtx) $
        evalReusabilityT $ RS.queryFldToPGAST fld actionExecuter
      resolvedAST <- RS.traverseQueryRootFldAST (resolveVal userInfo) unresolvedAST
      let txtSQL = Q.getQueryText $ RS.toPGQuery resolvedAST
          withExplain = "EXPLAIN (FORMAT TEXT) " <> txtSQL
      planLines <- liftTx $ map runIdentity <$>
                     Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
      return $ FieldPlan fName (Just txtSQL) $ Just planLines
  where
    fName = GV._fName fld

    queryCtxMap = _gQueryCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx

explainGQLQuery
  :: (MonadError QErr m, MonadIO m,HasVersion)
  => PGExecCtx
  -> SchemaCache
  -> SQLGenCtx
  -> Bool
  -> QueryActionExecuter
  -> GQLExplain
  -> m EncJSON
explainGQLQuery pgExecCtx sc sqlGenCtx enableAL actionExecuter (GQLExplain query userVarsRaw) = do
  (execPlan, queryReusability) <- runReusabilityT $
    E.getExecPlanPartial userInfo sc enableAL query
  (gCtx, rootSelSet) <- case execPlan of
    E.GExPHasura (gCtx, rootSelSet) ->
      return (gCtx, rootSelSet)
    E.GExPRemote _ _  ->
      throw400 InvalidParams "only hasura queries can be explained"
  case rootSelSet of
    GV.RQuery selSet ->
      runInTx $ encJFromJValue <$> traverse (explainField userInfo gCtx sqlGenCtx actionExecuter)
      (toList selSet)
    GV.RMutation _ ->
      throw400 InvalidParams "only queries can be explained"
    GV.RSubscription rootField -> do
      (plan, _) <- E.getSubsOp pgExecCtx gCtx sqlGenCtx userInfo queryReusability actionExecuter rootField
      runInTx $ encJFromJValue <$> E.explainLiveQueryPlan plan
  where
    usrVars = mkUserVars $ maybe [] Map.toList userVarsRaw
    userInfo = mkUserInfo (fromMaybe adminRole $ roleFromVars usrVars) usrVars
    runInTx = liftEither <=< liftIO . runExceptT . runLazyTx pgExecCtx Q.ReadOnly
