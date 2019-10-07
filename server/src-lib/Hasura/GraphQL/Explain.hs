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
import           Hasura.GraphQL.Resolve.Types
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Data.Sequence                          as Seq
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

type Explain r =
  (ReaderT r (Except QErr))

runExplain
  :: (MonadError QErr m)
  => r -> Explain r a -> m a
runExplain ctx m =
  either throwError return $ runExcept $ runReaderT m ctx

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
  :: (MonadTx m)
  => UserInfo -> GCtx -> SQLGenCtx -> GV.Field -> m FieldPlan
explainField userInfo gCtx sqlGenCtx fld =
  case fName of
    "__type"     -> return $ FieldPlan fName Nothing Nothing
    "__schema"   -> return $ FieldPlan fName Nothing Nothing
    "__typename" -> return $ FieldPlan fName Nothing Nothing
    _            -> do
      unresolvedAST <-
        runExplain (queryCtxMap, userInfo, fldMap, orderByCtx, sqlGenCtx) $
          evalResolveT $ RS.queryFldToPGAST fld
      resolvedAST <- RS.traverseQueryRootFldAST (resolveVal userInfo)
                     unresolvedAST
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
  :: (MonadError QErr m, MonadIO m)
  => PGExecCtx
  -> SchemaCache
  -> SQLGenCtx
  -> Bool
  -> GQLExplain
  -> m EncJSON
explainGQLQuery pgExecCtx sc sqlGenCtx enableAL (GQLExplain query userVarsRaw) = do
  opDef <- GV.getTypedOp opNameM selSets opDefs
  execPlans <- E.getExecPlanPartial userInfo sc enableAL query
  let mRootSelSet =
        foldl'
          (\accumSelSetM execPlan ->
             case execPlan of
               E.ExPHasuraPartial (gCtx, topField, _) -> case topField of
                 GV.HasuraTopQuery field -> fmap (\(gctx, fields) -> (gctx, (Seq.|>) fields field)) accumSelSetM
                 GV.HasuraTopMutation field -> fmap (\(gctx, fields) -> (gctx, (Seq.|>) fields field)) accumSelSetM
                 GV.HasuraTopSubscription field -> pure $ (gCtx, Seq.singleton field)
               E.ExPRemotePartial {} -> Nothing)
          (pure (emptyGCtx, Seq.empty))
          execPlans
  rootSelSet <- onNothing mRootSelSet $ throw400 InvalidParams "only hasura queries can be explained"
  case (G._todType opDef, rootSelSet) of
    (G.OperationTypeQuery, (gCtx, selSet)) ->
      runInTx $ encJFromJValue <$> traverse (explainField userInfo gCtx sqlGenCtx) (toList selSet)
    (G.OperationTypeMutation, _) ->
      throw400 InvalidParams "only queries can be explained"
    (G.OperationTypeSubscription, (gCtx, selSet)) -> do
      rootField <- getRootField selSet
      (plan, _) <- E.getSubsOp pgExecCtx gCtx sqlGenCtx userInfo rootField
      runInTx $ encJFromJValue <$> E.explainLiveQueryPlan plan
  where
    usrVars = mkUserVars $ maybe [] Map.toList userVarsRaw
    userInfo = mkUserInfo (fromMaybe adminRole $ roleFromVars usrVars) usrVars
    runInTx = liftEither <=< liftIO . runExceptT . runLazyTx pgExecCtx
    (GH.GQLReq opNameM q _) = query
    (selSets, opDefs, _) = G.partitionExDefs $ GH.unGQLExecDoc q
    getRootField = \case
      Seq.Empty -> throw400 InvalidParams "expected one top field in subscription"
      (fld Seq.:<| Seq.Empty) -> pure fld
      _ -> throw400 InvalidParams "expected only one top field in subscription"
