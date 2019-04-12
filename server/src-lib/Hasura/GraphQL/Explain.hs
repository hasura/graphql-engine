module Hasura.GraphQL.Explain
  ( explainGQLQuery
  , GQLExplain
  ) where

import           Data.Has                               (getter)

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Casing                      as J
import qualified Data.Aeson.TH                          as J
import qualified Data.HashMap.Strict                    as Map
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Text.Builder                           as TB

import           Hasura.EncJSON
import           Hasura.GraphQL.Context
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Resolve.Select          as RS
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV
import qualified Hasura.RQL.DML.Select                  as RS

data GQLExplain
  = GQLExplain
  { _gqeQuery :: !GH.GraphQLRequest
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

type Explain =
  (ReaderT (FieldMap, OrdByCtx, SQLGenCtx) (Except QErr))

runExplain
  :: (MonadError QErr m)
  => (FieldMap, OrdByCtx, SQLGenCtx) -> Explain a -> m a
runExplain ctx m =
  either throwError return $ runExcept $ runReaderT m ctx

explainField
  :: (MonadTx m)
  => UserInfo -> GCtx -> SQLGenCtx -> Field -> m FieldPlan
explainField userInfo gCtx sqlGenCtx fld =
  case fName of
    "__type"     -> return $ FieldPlan fName Nothing Nothing
    "__schema"   -> return $ FieldPlan fName Nothing Nothing
    "__typename" -> return $ FieldPlan fName Nothing Nothing
    _            -> do
      opCxt <- getOpCtx fName
      builderSQL <- runExplain (fldMap, orderByCtx, sqlGenCtx) $
        case opCxt of
          OCSelect (SelOpCtx tn hdrs permFilter permLimit) -> do
            validateHdrs hdrs
            toSQL . RS.mkSQLSelect False <$>
              RS.fromField txtConverter tn permFilter permLimit fld
          OCSelectPkey (SelPkOpCtx tn hdrs permFilter argMap) -> do
            validateHdrs hdrs
            toSQL . RS.mkSQLSelect True <$>
              RS.fromFieldByPKey txtConverter tn argMap permFilter fld
          OCSelectAgg (SelOpCtx tn hdrs permFilter permLimit) -> do
            validateHdrs hdrs
            toSQL . RS.mkAggSelect <$>
              RS.fromAggField txtConverter tn permFilter permLimit fld
          OCFuncQuery (FuncQOpCtx tn hdrs permFilter permLimit fn argSeq) ->
            procFuncQuery tn fn permFilter permLimit hdrs argSeq False
          OCFuncAggQuery (FuncQOpCtx tn hdrs permFilter permLimit fn argSeq) ->
            procFuncQuery tn fn permFilter permLimit hdrs argSeq True
          _ -> throw500 "unexpected mut field info for explain"

      let txtSQL = TB.run builderSQL
          withExplain = "EXPLAIN (FORMAT TEXT) " <> txtSQL
      planLines <- liftTx $ map runIdentity <$>
        Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
      return $ FieldPlan fName (Just txtSQL) $ Just planLines
  where
    fName = _fName fld

    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx

    getOpCtx f =
      onNothing (Map.lookup f opCtxMap) $ throw500 $
      "lookup failed: opctx: " <> showName f

    procFuncQuery tn fn permFilter permLimit hdrs argSeq isAgg = do
      validateHdrs hdrs
      (tabArgs, eSel, frmItem) <-
        RS.fromFuncQueryField txtConverter fn argSeq isAgg fld
      strfyNum <- stringifyNum <$> asks getter
      return $ toSQL $
        RS.mkFuncSelectWith fn tn
        (RS.TablePerm permFilter permLimit) tabArgs strfyNum eSel frmItem

    validateHdrs hdrs = do
      let receivedHdrs = userVars userInfo
      forM_ hdrs $ \hdr ->
        unless (isJust $ getVarVal hdr receivedHdrs) $
        throw400 NotFound $ hdr <<> " header is expected but not found"

explainGQLQuery
  :: (MonadError QErr m, MonadIO m)
  => Q.PGPool
  -> Q.TxIsolation
  -> SchemaCache
  -> SQLGenCtx
  -> GQLExplain
  -> m EncJSON
explainGQLQuery pool iso sc sqlGenCtx (GQLExplain query userVarsRaw)= do
  execPlan <- E.getExecPlan userInfo sc query
  let (E.GQExecPlan hasuraPlan _ _) = execPlan

  (gCtx, rootSelSet) <- case hasuraPlan of
    Just (E.GExPHasura gCtx rootSelSet _ ) ->
      return (gCtx, rootSelSet)
    _ -> throw400 InvalidParams "only hasura queries can be explained"
  case rootSelSet of
    GV.RQuery selSet -> do
      let tx = mapM (explainField userInfo gCtx sqlGenCtx) (toList selSet)
      plans <- liftIO (runExceptT $ runTx tx) >>= liftEither
      return $ encJFromJValue plans
    GV.RMutation _ ->
      throw400 InvalidParams "only queries can be explained"
    GV.RSubscription _ ->
      throw400 InvalidParams "only queries can be explained"
  where
    usrVars = mkUserVars $ maybe [] Map.toList userVarsRaw
    userInfo = mkUserInfo (fromMaybe adminRole $ roleFromVars usrVars) usrVars

    runTx tx = runLazyTx pool iso $ withUserInfo userInfo tx
