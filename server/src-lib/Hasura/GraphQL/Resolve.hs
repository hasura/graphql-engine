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

  , RIntro.schemaR
  , RIntro.typeR
  ) where

import           Data.Has
import           Hasura.Session

import qualified Data.HashMap.Strict               as Map
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G
import qualified Network.HTTP.Client               as HTTP
import qualified Network.HTTP.Types                as HTTP

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
import qualified Hasura.GraphQL.Validate           as V
import qualified Hasura.RQL.DML.Select             as DS
import qualified Hasura.SQL.DML                    as S

data QueryRootFldAST v
  = QRFPk !(DS.AnnSimpleSelG v)
  | QRFSimple !(DS.AnnSimpleSelG v)
  | QRFAgg !(DS.AnnAggSelG v)
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
  QRFPk s                  -> QRFPk <$> DS.traverseAnnSimpleSel f s
  QRFSimple s              -> QRFSimple <$> DS.traverseAnnSimpleSel f s
  QRFAgg s                 -> QRFAgg <$> DS.traverseAnnAggSel f s
  QRFActionSelect s        -> QRFActionSelect <$> DS.traverseAnnSimpleSel f s
  QRFActionExecuteObject s -> QRFActionExecuteObject <$> DS.traverseAnnSimpleSel f s
  QRFActionExecuteList s   -> QRFActionExecuteList <$> DS.traverseAnnSimpleSel f s

toPGQuery :: QueryRootFldResolved -> Q.Query
toPGQuery = \case
  QRFPk s                  -> DS.selectQuerySQL DS.JASSingleObject s
  QRFSimple s              -> DS.selectQuerySQL DS.JASMultipleRows s
  QRFAgg s                 -> DS.selectAggQuerySQL s
  QRFActionSelect s        -> DS.selectQuerySQL DS.JASSingleObject s
  QRFActionExecuteObject s -> DS.selectQuerySQL DS.JASSingleObject s
  QRFActionExecuteList s   -> DS.selectQuerySQL DS.JASMultipleRows s

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
     )
  => V.Field
  -> RA.QueryActionExecuter
  -> m QueryRootFldUnresolved
queryFldToPGAST fld actionExecuter = do
  opCtx <- getOpCtx $ V._fName fld
  userInfo <- asks getter
  case opCtx of
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
      let f = case jsonAggType of
             DS.JASMultipleRows -> QRFActionExecuteList
             DS.JASSingleObject -> QRFActionExecuteObject
      f <$> actionExecuter (RA.resolveActionQuery fld ctx (_uiSession userInfo))
      where
        outputType = _saecOutputType ctx
        jsonAggType = RA.mkJsonAggSelect outputType

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
     )
  => V.Field
  -> m (RespTx, HTTP.ResponseHeaders)
mutFldToTx fld = do
  userInfo <- asks getter
  opCtx <- getOpCtx $ V._fName fld
  let noRespHeaders = fmap (,[])
      roleName = _uiRole userInfo
  case opCtx of
    MCInsert ctx -> do
      validateHdrs userInfo (_iocHeaders ctx)
      noRespHeaders $ RI.convertInsert roleName (_iocTable ctx) fld
    MCInsertOne ctx -> do
      validateHdrs userInfo (_iocHeaders ctx)
      noRespHeaders $ RI.convertInsertOne roleName (_iocTable ctx) fld
    MCUpdate ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      noRespHeaders $ RM.convertUpdate ctx fld
    MCUpdateByPk ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      noRespHeaders $ RM.convertUpdateByPk ctx fld
    MCDelete ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      noRespHeaders $ RM.convertDelete ctx fld
    MCDeleteByPk ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      noRespHeaders $ RM.convertDeleteByPk ctx fld
    MCAction ctx ->
      RA.resolveActionMutation fld ctx (_uiSession userInfo)

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
