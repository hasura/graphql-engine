module Hasura.GraphQL.Resolve
  ( mutFldToTx

  , queryFldToPGAST
  , traverseQueryRootFldAST
  , UnresolvedVal(..)

  , AnnPGVal(..)
  , txtConverter

  , QueryRootFldAST(..)
  , QueryRootFldUnresolved
  , QueryRootFldResolved
  , toPGQuery
  , toSQLFromItem

  , RIntro.schemaR
  , RIntro.typeR
  ) where

import           Data.Has
import           Hasura.Session

import qualified Data.Environment                  as Env
import qualified Data.HashMap.Strict               as Map
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G
import qualified Network.HTTP.Client               as HTTP
import qualified Network.HTTP.Types                as HTTP

import           Hasura.EncJSON
import           Hasura.GraphQL.Resolve.Context
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Version             (HasVersion)
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.Action     as RA
import qualified Hasura.GraphQL.Resolve.Insert     as RI
import qualified Hasura.GraphQL.Resolve.Introspect as RIntro
import qualified Hasura.GraphQL.Resolve.Mutation   as RM
import qualified Hasura.GraphQL.Resolve.Select     as RS
import qualified Hasura.GraphQL.Schema.Common      as GS
import qualified Hasura.GraphQL.Validate           as V
import qualified Hasura.RQL.DML.RemoteJoin         as RR
import qualified Hasura.RQL.DML.Select             as DS
import qualified Hasura.SQL.DML                    as S
import qualified Hasura.Tracing                    as Tracing

data QueryRootFldAST v
  = QRFNode !(DS.AnnSimpleSelG v)
  | QRFPk !(DS.AnnSimpleSelG v)
  | QRFSimple !(DS.AnnSimpleSelG v)
  | QRFAgg !(DS.AnnAggregateSelectG v)
  | QRFConnection !(DS.ConnectionSelect v)
  | QRFActionSelect !(DS.AnnSimpleSelG v)
  | QRFActionExecuteObject !(DS.AnnSimpleSelG v)
  | QRFActionExecuteList !(DS.AnnSimpleSelG v)
  deriving (Show, Eq)

type QueryRootFldUnresolved = QueryRootFldAST UnresolvedVal
type QueryRootFldResolved = QueryRootFldAST S.SQLExp

traverseQueryRootFldAST
  :: (Applicative f)
  => (a -> f b)
  -> QueryRootFldAST a
  -> f (QueryRootFldAST b)
traverseQueryRootFldAST f = \case
  QRFNode s                -> QRFNode <$> DS.traverseAnnSimpleSelect f s
  QRFPk s                  -> QRFPk <$> DS.traverseAnnSimpleSelect f s
  QRFSimple s              -> QRFSimple <$> DS.traverseAnnSimpleSelect f s
  QRFAgg s                 -> QRFAgg <$> DS.traverseAnnAggregateSelect f s
  QRFActionSelect s        -> QRFActionSelect <$> DS.traverseAnnSimpleSelect f s
  QRFActionExecuteObject s -> QRFActionExecuteObject <$> DS.traverseAnnSimpleSelect f s
  QRFActionExecuteList s   -> QRFActionExecuteList <$> DS.traverseAnnSimpleSelect f s
  QRFConnection s          -> QRFConnection <$> DS.traverseConnectionSelect f s

toPGQuery :: QueryRootFldResolved -> (Q.Query, Maybe RR.RemoteJoins)
toPGQuery = \case
  QRFNode s                -> first (toQuery . DS.mkSQLSelect DS.JASSingleObject) $ RR.getRemoteJoins s
  QRFPk s                  -> first (toQuery . DS.mkSQLSelect DS.JASSingleObject) $ RR.getRemoteJoins s
  QRFSimple s              -> first (toQuery . DS.mkSQLSelect DS.JASMultipleRows) $ RR.getRemoteJoins s
  QRFAgg s                 -> first (toQuery . DS.mkAggregateSelect) $ RR.getRemoteJoinsAggregateSelect s
  QRFActionSelect s        -> first (toQuery . DS.mkSQLSelect DS.JASSingleObject) $ RR.getRemoteJoins s
  QRFActionExecuteObject s -> first (toQuery . DS.mkSQLSelect DS.JASSingleObject) $ RR.getRemoteJoins s
  QRFActionExecuteList s   -> first (toQuery . DS.mkSQLSelect DS.JASMultipleRows) $ RR.getRemoteJoins s
  QRFConnection s          -> first (toQuery . DS.mkConnectionSelect) $ RR.getRemoteJoinsConnectionSelect s
  where
    toQuery :: ToSQL a => a -> Q.Query
    toQuery = Q.fromBuilder . toSQL

validateHdrs
  :: (Foldable t, QErrM m) => UserInfo -> t Text -> m ()
validateHdrs userInfo hdrs = do
  let receivedVars = _uiSession userInfo
  forM_ hdrs $ \hdr ->
    unless (isJust $ getSessionVariableValue (mkSessionVariable hdr) receivedVars) $
    throw400 NotFound $ hdr <<> " header is expected but not found"

queryFldToPGAST
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has UserInfo r
     , Has QueryCtxMap r
     , HasVersion
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> V.Field
  -> RA.QueryActionExecuter
  -> m QueryRootFldUnresolved
queryFldToPGAST env fld actionExecuter = do
  opCtx <- getOpCtx $ V._fName fld
  userInfo <- asks getter
  case opCtx of
    QCNodeSelect nodeSelectMap -> do
      NodeIdV1 (V1NodeId table columnValues) <- RS.resolveNodeId fld
      case Map.lookup (GS.mkTableTy table) nodeSelectMap of
        Nothing       -> throwVE $ "table " <> table <<> " not found"
        Just (selOpCtx, pkeyColumns) -> do
          validateHdrs userInfo (_socHeaders selOpCtx)
          QRFNode <$> RS.convertNodeSelect selOpCtx pkeyColumns columnValues fld
    QCSelect ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      QRFSimple <$> RS.convertSelect ctx fld
    QCSelectPkey ctx -> do
      validateHdrs userInfo (_spocHeaders ctx)
      QRFPk <$> RS.convertSelectByPKey ctx fld
    QCSelectAgg ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      QRFAgg <$> RS.convertAggSelect ctx fld
    QCFuncQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      QRFSimple <$> RS.convertFuncQuerySimple ctx fld
    QCFuncAggQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      QRFAgg <$> RS.convertFuncQueryAgg ctx fld
    QCAsyncActionFetch ctx ->
      QRFActionSelect <$> RA.resolveAsyncActionQuery userInfo ctx fld
    QCAction ctx -> do
      -- query actions should not be marked reusable because we aren't
      -- capturing the variable value in the state as re-usable variables.
      -- The variables captured in non-action queries are used to generate
      -- an SQL query, but in case of query actions it's converted into JSON
      -- and included in the action's webhook payload.
      markNotReusable
      let jsonAggType = RA.mkJsonAggSelect $ _saecOutputType ctx
          f = case jsonAggType of
             DS.JASMultipleRows -> QRFActionExecuteList
             DS.JASSingleObject -> QRFActionExecuteObject
      f <$> actionExecuter (RA.resolveActionQuery env fld ctx (_uiSession userInfo))
    QCSelectConnection pk ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      QRFConnection <$> RS.convertConnectionSelect pk ctx fld
    QCFuncConnection pk ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      QRFConnection <$> RS.convertConnectionFuncQuery pk ctx fld

mutFldToTx
  :: ( HasVersion
     , MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has MutationCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     , Has HTTP.Manager r
     , Has [HTTP.Header] r
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadIO tx
     , MonadTx tx
     , Tracing.MonadTrace tx
     )
  => Env.Environment
  -> V.Field
  -> m (tx EncJSON, HTTP.ResponseHeaders)
mutFldToTx env fld = do
  userInfo <- asks getter
  reqHeaders <- asks getter
  httpManager <- asks getter
  let rjCtx = (httpManager, reqHeaders, userInfo)
  opCtx <- getOpCtx $ V._fName fld
  let noRespHeaders = fmap (,[])
      roleName = _uiRole userInfo
  case opCtx of
    MCInsert ctx -> do
      validateHdrs userInfo (_iocHeaders ctx)
      noRespHeaders $ RI.convertInsert env rjCtx roleName (_iocTable ctx) fld
    MCInsertOne ctx -> do
      validateHdrs userInfo (_iocHeaders ctx)
      noRespHeaders $ RI.convertInsertOne env rjCtx roleName (_iocTable ctx) fld
    MCUpdate ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      noRespHeaders $ RM.convertUpdate env ctx rjCtx fld
    MCUpdateByPk ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      noRespHeaders $ RM.convertUpdateByPk env ctx rjCtx fld
    MCDelete ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      noRespHeaders $ RM.convertDelete env ctx rjCtx fld
    MCDeleteByPk ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      noRespHeaders $ RM.convertDeleteByPk env ctx rjCtx fld
    MCAction ctx ->
      RA.resolveActionMutation env fld ctx userInfo

getOpCtx
  :: ( MonadReusability m
     , MonadError QErr m
     , MonadReader r m
     , Has (OpCtxMap a) r
     )
  => G.Name -> m a
getOpCtx f = do
  opCtxMap <- asks getter
  onNothing (Map.lookup f opCtxMap) $ throw500 $
    "lookup failed: opctx: " <> showName f

toSQLFromItem :: S.Alias -> QueryRootFldResolved -> S.FromItem
toSQLFromItem alias = \case
  QRFNode s                -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  QRFPk s                  -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  QRFSimple s              -> fromSelect $ DS.mkSQLSelect DS.JASMultipleRows s
  QRFAgg s                 -> fromSelect $ DS.mkAggregateSelect s
  QRFActionSelect s        -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  QRFActionExecuteObject s -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  QRFActionExecuteList s   -> fromSelect $ DS.mkSQLSelect DS.JASSingleObject s
  QRFConnection s          -> flip (S.FISelectWith (S.Lateral False)) alias
                              $ DS.mkConnectionSelect s
  where
    fromSelect = flip (S.FISelect (S.Lateral False)) alias
