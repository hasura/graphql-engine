{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.Server.Query where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax   (Lift)

import qualified Data.ByteString.Builder      as BB
import qualified Data.ByteString.Lazy         as BL
import qualified Data.HashMap.Strict          as Map
import qualified Data.Sequence                as Seq
import qualified Data.Vector                  as V

import           Hasura.Prelude
import           Hasura.RQL.DDL.Metadata
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.QueryTemplate
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.Schema.Table
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.DML.Returning     (encodeJSONVector)
import           Hasura.RQL.Types
import           Hasura.Server.Utils
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
  -> q
  -> Either QErr (Q.TxE QErr (BL.ByteString, SchemaCache))
buildTx userInfo sc q = do
  p1Res <- withPathK "args" $ runP1 qEnv $ phaseOne q
  return $ flip runReaderT (qcUserInfo qEnv) $
    flip runStateT sc $ withPathK "args" $ phaseTwo q p1Res
  where
    qEnv = QCtx userInfo sc

runQuery
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> SchemaCache
  -> RQLQuery -> m (BL.ByteString, SchemaCache)
runQuery pool isoL userInfo sc query = do
  tx <- liftEither $ buildTxAny userInfo sc query
  res <- liftIO $ runExceptT $ Q.runTx pool (isoL, Nothing) $
         setHeadersTx userInfo >> tx
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

buildTxAny :: UserInfo
           -> SchemaCache
           -> RQLQuery
           -> Either QErr (Q.TxE QErr (BL.ByteString, SchemaCache))
buildTxAny userInfo sc rq = case rq of
  RQAddExistingTableOrView q    -> buildTx userInfo sc q
  RQTrackTable q                -> buildTx userInfo sc q
  RQUntrackTable q              -> buildTx userInfo sc q

  RQCreateObjectRelationship q -> buildTx userInfo sc q
  RQCreateArrayRelationship  q -> buildTx userInfo sc q
  RQDropRelationship  q        -> buildTx userInfo sc q
  RQSetRelationshipComment  q  -> buildTx userInfo sc q

  RQCreateInsertPermission q -> buildTx userInfo sc q
  RQCreateSelectPermission q -> buildTx userInfo sc q
  RQCreateUpdatePermission q -> buildTx userInfo sc q
  RQCreateDeletePermission q -> buildTx userInfo sc q

  RQDropInsertPermission q -> buildTx userInfo sc q
  RQDropSelectPermission q -> buildTx userInfo sc q
  RQDropUpdatePermission q -> buildTx userInfo sc q
  RQDropDeletePermission q -> buildTx userInfo sc q
  RQSetPermissionComment q -> buildTx userInfo sc q

  RQInsert q -> buildTx userInfo sc q
  RQSelect q -> buildTx userInfo sc q
  RQUpdate q -> buildTx userInfo sc q
  RQDelete q -> buildTx userInfo sc q
  RQCount q  -> buildTx userInfo sc q

  RQCreateEventTrigger q -> buildTx userInfo sc q
  RQDeleteEventTrigger q -> buildTx userInfo sc q
  RQDeliverEvent q -> buildTx userInfo sc q

  RQCreateQueryTemplate q     -> buildTx userInfo sc q
  RQDropQueryTemplate q       -> buildTx userInfo sc q
  RQExecuteQueryTemplate q    -> buildTx userInfo sc q
  RQSetQueryTemplateComment q -> buildTx userInfo sc q

  RQReplaceMetadata q -> buildTx userInfo sc q
  RQClearMetadata q -> buildTx userInfo sc q
  RQExportMetadata q -> buildTx userInfo sc q
  RQReloadMetadata q -> buildTx userInfo sc q

  RQDumpInternalState q -> buildTx userInfo sc q

  RQRunSql q -> buildTx userInfo sc q

  RQBulk qs  ->
    let f (respList, scf) q = do
          dbAction <- liftEither $ buildTxAny userInfo scf q
          (resp, newSc) <- dbAction
          return ((Seq.|>) respList resp, newSc)
    in
      return $ withPathK "args" $ do
        (respList, finalSc) <- indexedFoldM f (Seq.empty, sc) qs
        let bsVector = V.fromList $ toList respList
        return ( BB.toLazyByteString $ encodeJSONVector BB.lazyByteString bsVector
               , finalSc
               )

setHeadersTx :: UserInfo -> Q.TxE QErr ()
setHeadersTx userInfo =
  forM_ hdrs $ \h -> Q.unitQE defaultTxErrorHandler (mkQ h) () False
  where
    hdrs = Map.toList $ Map.delete accessKeyHeader
      $ userHeaders userInfo
    mkQ (h, v) = Q.fromText $
      "SET LOCAL hasura." <> dropAndSnakeCase h <> " =  " <> pgFmtLit v
