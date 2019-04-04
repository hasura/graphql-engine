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
import           Hasura.GraphQL.Resolve.Context
import           Hasura.GraphQL.Validate.Field
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Resolve                 as RS
import qualified Hasura.GraphQL.Transport.HTTP.Protocol as GH
import qualified Hasura.GraphQL.Validate                as GV

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

explainField
  :: (MonadTx m)
  => UserInfo -> GCtx -> SQLGenCtx -> Field -> m FieldPlan
explainField userInfo gCtx sqlGenCtx fld =
  case fName of
    "__type"     -> return $ FieldPlan fName Nothing Nothing
    "__schema"   -> return $ FieldPlan fName Nothing Nothing
    "__typename" -> return $ FieldPlan fName Nothing Nothing
    _            -> do
      sqlQuery <- runExplain (opCtxMap, userInfo, fldMap, orderByCtx, sqlGenCtx) $
        RS.queryFldToSQL txtConverter fld
      let txtSQL = Q.getQueryText sqlQuery
          withExplain = "EXPLAIN (FORMAT TEXT) " <> txtSQL
      planLines <- liftTx $ map runIdentity <$>
        Q.listQE dmlTxErrorHandler (Q.fromText withExplain) () True
      return $ FieldPlan fName (Just txtSQL) $ Just planLines
  where
    fName = _fName fld

    opCtxMap = _gOpCtxMap gCtx
    fldMap = _gFields gCtx
    orderByCtx = _gOrdByCtx gCtx

explainGQLQuery
  :: (MonadError QErr m, MonadIO m)
  => Q.PGPool
  -> Q.TxIsolation
  -> SchemaCache
  -> SQLGenCtx
  -> GQLExplain
  -> m EncJSON
explainGQLQuery pool iso sc sqlGenCtx (GQLExplain query userVarsRaw)= do
  execPlan <- E.getExecPlanPartial userInfo sc query
  (gCtx, rootSelSet) <- case execPlan of
    E.GExPHasura (gCtx, rootSelSet, _) ->
      return (gCtx, rootSelSet)
    E.GExPRemote _ _  ->
      throw400 InvalidParams "only hasura queries can be explained"
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
