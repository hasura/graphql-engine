module Hasura.GraphQL.Resolve
  ( mutFldToTx
  , queryFldToPGAST
  , RS.traverseQueryRootFldAST
  , RS.toPGQuery
  , UnresolvedVal(..)

  , AnnPGVal(..)
  , txtConverter

  , RS.QueryRootFldUnresolved
  , resolveValPrep
  , queryFldToSQL
  , RIntro.schemaR
  , RIntro.typeR
  ) where

import           Data.Has

import qualified Data.HashMap.Strict               as Map
import qualified Database.PG.Query                 as Q
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.GraphQL.Resolve.Context
import           Hasura.Prelude
import           Hasura.RQL.DML.Internal           (sessVarFromCurrentSetting)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Resolve.Insert     as RI
import qualified Hasura.GraphQL.Resolve.Introspect as RIntro
import qualified Hasura.GraphQL.Resolve.Mutation   as RM
import qualified Hasura.GraphQL.Resolve.Select     as RS
import qualified Hasura.GraphQL.Validate           as V

validateHdrs
  :: (Foldable t, QErrM m) => UserInfo -> t Text -> m ()
validateHdrs userInfo hdrs = do
  let receivedVars = userVars userInfo
  forM_ hdrs $ \hdr ->
    unless (isJust $ getVarVal hdr receivedVars) $
    throw400 NotFound $ hdr <<> " header is expected but not found"

queryFldToPGAST
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has UserInfo r
     , Has OpCtxMap r
     )
  => V.Field
  -> m RS.QueryRootFldUnresolved
queryFldToPGAST fld = do
  opCtx <- getOpCtx $ V._fName fld
  userInfo <- asks getter
  case opCtx of
    OCSelect ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      RS.convertSelect ctx fld
    OCSelectPkey ctx -> do
      validateHdrs userInfo (_spocHeaders ctx)
      RS.convertSelectByPKey ctx fld
    OCSelectAgg ctx -> do
      validateHdrs userInfo (_socHeaders ctx)
      RS.convertAggSelect ctx fld
    OCFuncQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      RS.convertFuncQuerySimple ctx fld
    OCFuncAggQuery ctx -> do
      validateHdrs userInfo (_fqocHeaders ctx)
      RS.convertFuncQueryAgg ctx fld
    OCInsert _ ->
      throw500 "unexpected OCInsert for query field context"
    OCUpdate _ ->
      throw500 "unexpected OCUpdate for query field context"
    OCDelete _ ->
      throw500 "unexpected OCDelete for query field context"

queryFldToSQL
  :: ( MonadError QErr m, MonadReader r m, Has FieldMap r
     , Has OrdByCtx r, Has SQLGenCtx r, Has UserInfo r
     , Has OpCtxMap r
     )
  => PrepFn m
  -> V.Field
  -> m Q.Query
queryFldToSQL fn fld = do
  pgAST <- queryFldToPGAST fld
  resolvedAST <- flip RS.traverseQueryRootFldAST pgAST $ \case
    UVPG annPGVal -> fn annPGVal
    UVSQL sqlExp  -> return sqlExp
    UVSessVar colTy sessVar -> sessVarFromCurrentSetting colTy sessVar
  return $ RS.toPGQuery resolvedAST

mutFldToTx
  :: ( MonadError QErr m
     , MonadReader r m
     , Has UserInfo r
     , Has OpCtxMap r
     , Has FieldMap r
     , Has OrdByCtx r
     , Has SQLGenCtx r
     , Has InsCtxMap r
     )
  => V.Field
  -> m RespTx
mutFldToTx fld = do
  userInfo <- asks getter
  opCtx <- getOpCtx $ V._fName fld
  case opCtx of
    OCInsert ctx -> do
      let roleName = userRole userInfo
      validateHdrs userInfo (_iocHeaders ctx)
      RI.convertInsert roleName (_iocTable ctx) fld
    OCUpdate ctx -> do
      validateHdrs userInfo (_uocHeaders ctx)
      RM.convertUpdate ctx fld
    OCDelete ctx -> do
      validateHdrs userInfo (_docHeaders ctx)
      RM.convertDelete ctx fld
    OCSelect _ ->
      throw500 "unexpected query field context for a mutation field"
    OCSelectPkey _ ->
      throw500 "unexpected query field context for a mutation field"
    OCSelectAgg _ ->
      throw500 "unexpected query field context for a mutation field"
    OCFuncQuery _ ->
      throw500 "unexpected query field context for a mutation field"
    OCFuncAggQuery _ ->
      throw500 "unexpected query field context for a mutation field"

getOpCtx
  :: ( MonadError QErr m
     , MonadReader r m
     , Has OpCtxMap r
     )
  => G.Name -> m OpCtx
getOpCtx f = do
  opCtxMap <- asks getter
  onNothing (Map.lookup f opCtxMap) $ throw500 $
    "lookup failed: opctx: " <> showName f
