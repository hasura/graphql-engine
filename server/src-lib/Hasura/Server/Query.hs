{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.Server.Query where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax   (Lift)

import qualified Data.Aeson.Text              as AT
import qualified Data.ByteString.Builder      as BB
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Sequence                as Seq
import qualified Data.Text.Lazy               as LT
import qualified Data.Vector                  as V
import qualified Network.HTTP.Client          as HTTP

import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.QueryTemplate
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteSchema
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.DML.Returning     (encodeJSONVector)
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import qualified Database.PG.Query            as Q

data RQLQuery
  = RQAddExistingTableOrView !TrackTable
  | RQTrackTable !TrackTable
  | RQUntrackTable !UntrackTable

  | RQCreateObjectRelationship !CreateObjRel
  | RQCreateArrayRelationship !CreateArrRel
  | RQDropRelationship !DropRel
  | RQSetRelationshipComment !SetRelComment

  | RQCreateInsertPermission !CreateInsPerm
  | RQCreateSelectPermission !CreateSelPerm
  | RQCreateUpdatePermission !CreateUpdPerm
  | RQCreateDeletePermission !CreateDelPerm

  | RQDropInsertPermission !DropInsPerm
  | RQDropSelectPermission !DropSelPerm
  | RQDropUpdatePermission !DropUpdPerm
  | RQDropDeletePermission !DropDelPerm
  | RQSetPermissionComment !SetPermComment

  | RQInsert !InsertQuery
  | RQSelect !SelectQuery
  | RQUpdate !UpdateQuery
  | RQDelete !DeleteQuery
  | RQCount !CountQuery
  | RQBulk ![RQLQuery]

  -- schema-stitching, custom resolver related
  | RQAddRemoteSchema !AddRemoteSchemaQuery
  | RQRemoveRemoteSchema !RemoveRemoteSchemaQuery

  | RQCreateEventTrigger !CreateEventTriggerQuery
  | RQDeleteEventTrigger !DeleteEventTriggerQuery
  | RQDeliverEvent       !DeliverEventQuery

  | RQCreateQueryTemplate !CreateQueryTemplate
  | RQDropQueryTemplate !DropQueryTemplate
  | RQExecuteQueryTemplate !ExecQueryTemplate
  | RQSetQueryTemplateComment !SetQueryTemplateComment

  | RQRunSql !RunSQL

  | RQReplaceMetadata !ReplaceMetadata
  | RQExportMetadata !ExportMetadata
  | RQClearMetadata !ClearMetadata
  | RQReloadMetadata !ReloadMetadata

  | RQDumpInternalState !DumpInternalState

  deriving (Show, Eq, Lift)

$(deriveJSON
  defaultOptions { constructorTagModifier = snakeCase . drop 2
                 , sumEncoding = TaggedObject "type" "args"
                 }
  ''RQLQuery)

buildTx
  :: (HDBQuery q)
  => UserInfo
  -> SchemaCache
  -> HTTP.Manager
  -> q
  -> Either QErr (Q.TxE QErr (BL.ByteString, SchemaCache))
buildTx userInfo sc httpManager q = do
  p1Res <- withPathK "args" $ runP1 qEnv $ phaseOne q
  return $ flip runReaderT p2Ctx $
    flip runStateT sc $ withPathK "args" $ phaseTwo q p1Res
  where
    p2Ctx = P2Ctx userInfo httpManager
    qEnv = QCtx userInfo sc

runQuery
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> SchemaCache -> HTTP.Manager
  -> RQLQuery -> m (BL.ByteString, SchemaCache)
runQuery pool isoL userInfo sc hMgr query = do
  tx <- liftEither $ buildTxAny userInfo sc hMgr query
  res <- liftIO $ runExceptT $ Q.runTx pool (isoL, Nothing) $
         setHeadersTx (userVars userInfo) >> tx
  liftEither res

queryNeedsReload :: RQLQuery -> Bool
queryNeedsReload qi = case qi of
  RQAddExistingTableOrView q   -> queryModifiesSchema q
  RQTrackTable q               -> queryModifiesSchema q
  RQUntrackTable q             -> queryModifiesSchema q

  RQCreateObjectRelationship q -> queryModifiesSchema q
  RQCreateArrayRelationship  q -> queryModifiesSchema q
  RQDropRelationship  q        -> queryModifiesSchema q
  RQSetRelationshipComment  q  -> queryModifiesSchema q

  RQCreateInsertPermission q   -> queryModifiesSchema q
  RQCreateSelectPermission q   -> queryModifiesSchema q
  RQCreateUpdatePermission q   -> queryModifiesSchema q
  RQCreateDeletePermission q   -> queryModifiesSchema q

  RQDropInsertPermission q     -> queryModifiesSchema q
  RQDropSelectPermission q     -> queryModifiesSchema q
  RQDropUpdatePermission q     -> queryModifiesSchema q
  RQDropDeletePermission q     -> queryModifiesSchema q
  RQSetPermissionComment q     -> queryModifiesSchema q

  RQInsert q                   -> queryModifiesSchema q
  RQSelect q                   -> queryModifiesSchema q
  RQUpdate q                   -> queryModifiesSchema q
  RQDelete q                   -> queryModifiesSchema q
  RQCount q                    -> queryModifiesSchema q

  RQAddRemoteSchema q          -> queryModifiesSchema q
  RQRemoveRemoteSchema q       -> queryModifiesSchema q

  RQCreateEventTrigger q       -> queryModifiesSchema q
  RQDeleteEventTrigger q       -> queryModifiesSchema q
  RQDeliverEvent q             -> queryModifiesSchema q

  RQCreateQueryTemplate q      -> queryModifiesSchema q
  RQDropQueryTemplate q        -> queryModifiesSchema q
  RQExecuteQueryTemplate q     -> queryModifiesSchema q
  RQSetQueryTemplateComment q  -> queryModifiesSchema q

  RQRunSql q                   -> queryModifiesSchema q

  RQReplaceMetadata q          -> queryModifiesSchema q
  RQExportMetadata q           -> queryModifiesSchema q
  RQClearMetadata q            -> queryModifiesSchema q
  RQReloadMetadata q           -> queryModifiesSchema q

  RQDumpInternalState q        -> queryModifiesSchema q

  RQBulk qs                    -> any queryNeedsReload qs

buildTxAny
  :: UserInfo
  -> SchemaCache
  -> HTTP.Manager
  -> RQLQuery
  -> Either QErr (Q.TxE QErr (BL.ByteString, SchemaCache))
buildTxAny userInfo sc hMgr rq = case rq of
  RQAddExistingTableOrView q -> buildTx' q
  RQTrackTable q             -> buildTx' q
  RQUntrackTable q           -> buildTx' q

  RQCreateObjectRelationship q -> buildTx' q
  RQCreateArrayRelationship  q -> buildTx' q
  RQDropRelationship  q        -> buildTx' q
  RQSetRelationshipComment  q  -> buildTx' q

  RQCreateInsertPermission q -> buildTx' q
  RQCreateSelectPermission q -> buildTx' q
  RQCreateUpdatePermission q -> buildTx' q
  RQCreateDeletePermission q -> buildTx' q

  RQDropInsertPermission q -> buildTx' q
  RQDropSelectPermission q -> buildTx' q
  RQDropUpdatePermission q -> buildTx' q
  RQDropDeletePermission q -> buildTx' q
  RQSetPermissionComment q -> buildTx' q

  RQInsert q -> buildTx' q
  RQSelect q -> buildTx' q
  RQUpdate q -> buildTx' q
  RQDelete q -> buildTx' q
  RQCount  q -> buildTx' q

  RQAddRemoteSchema    q -> buildTx' q
  RQRemoveRemoteSchema q -> buildTx' q

  RQCreateEventTrigger q -> buildTx' q
  RQDeleteEventTrigger q -> buildTx' q
  RQDeliverEvent q       -> buildTx' q

  RQCreateQueryTemplate q     -> buildTx' q
  RQDropQueryTemplate q       -> buildTx' q
  RQExecuteQueryTemplate q    -> buildTx' q
  RQSetQueryTemplateComment q -> buildTx' q

  RQReplaceMetadata q -> buildTx' q
  RQClearMetadata q   -> buildTx' q
  RQExportMetadata q  -> buildTx' q
  RQReloadMetadata q  -> buildTx' q

  RQDumpInternalState q -> buildTx' q

  RQRunSql q -> buildTx' q

  RQBulk qs ->
    let f (respList, scf) q = do
          dbAction <- liftEither $ buildTxAny userInfo scf hMgr q
          (resp, newSc) <- dbAction
          return ((Seq.|>) respList resp, newSc)
    in
      return $ withPathK "args" $ do
        (respList, finalSc) <- indexedFoldM f (Seq.empty, sc) qs
        let bsVector = V.fromList $ toList respList
        return ( BB.toLazyByteString $ encodeJSONVector BB.lazyByteString bsVector
               , finalSc
               )

  where buildTx' q = buildTx userInfo sc hMgr q

setHeadersTx :: UserVars -> Q.TxE QErr ()
setHeadersTx uVars =
  Q.unitQE defaultTxErrorHandler setSess () False
  where
    toStrictText = LT.toStrict . AT.encodeToLazyText
    setSess = Q.fromText $
      "SET LOCAL \"hasura.user\" = " <>
      pgFmtLit (toStrictText uVars)
