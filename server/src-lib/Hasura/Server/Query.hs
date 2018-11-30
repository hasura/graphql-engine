{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Hasura.Server.Query where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax   (Lift)

import qualified Data.Aeson.Text              as AT
import qualified Data.ByteString.Builder      as BB
import qualified Data.ByteString.Lazy         as BL
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
import           Hasura.RQL.DDL.Subscribe
import           Hasura.RQL.DML.Count
import           Hasura.RQL.DML.Delete
import           Hasura.RQL.DML.Insert
import           Hasura.RQL.DML.QueryTemplate
import           Hasura.RQL.DML.Returning     (encodeJSONVector)
import           Hasura.RQL.DML.Select
import           Hasura.RQL.DML.Update
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

data LazyTx e a
  = LTErr e
  | LTNoTx a
  | LTTx (Q.TxE e a)

lazyTxToQTx :: LazyTx e a -> Q.TxE e a
lazyTxToQTx = \case
  LTErr e  -> throwError e
  LTNoTx r -> return r
  LTTx tx  -> tx

instance Functor (LazyTx e) where
  fmap f = \case
    LTErr e  -> LTErr e
    LTNoTx a -> LTNoTx $ f a
    LTTx tx  -> LTTx $ fmap f tx

instance Applicative (LazyTx e) where
  pure = LTNoTx

  LTErr e   <*> _         = LTErr e
  LTNoTx f  <*> r         = fmap f r
  LTTx _    <*> LTErr e   = LTErr e
  LTTx txf  <*> LTNoTx a  = LTTx $ txf <*> pure a
  LTTx txf  <*> LTTx tx   = LTTx $ txf <*> tx

instance Monad (LazyTx e) where
  LTErr e >>= _  = LTErr e
  LTNoTx a >>= f = f a
  LTTx txa >>= f =
    LTTx $ txa >>= lazyTxToQTx . f

instance MonadError e (LazyTx e) where
  throwError = LTErr
  LTErr e  `catchError` f = f e
  LTNoTx a `catchError` _ = LTNoTx a
  LTTx txe `catchError` f =
    LTTx $ txe `catchError` (lazyTxToQTx . f)

instance MonadTx (LazyTx QErr) where
  liftTx = LTTx

instance MonadIO (LazyTx QErr) where
  liftIO = LTTx . liftIO

newtype Run a
  = Run {unRun :: StateT SchemaCache (ReaderT (UserInfo, HTTP.Manager) (LazyTx QErr)) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadState SchemaCache
           , MonadReader (UserInfo, HTTP.Manager)
           , CacheRM
           , CacheRWM
           , MonadTx
           , MonadIO
           )

instance UserInfoM Run where
  askUserInfo = asks fst

instance HasHttpManager Run where
  askHttpManager = asks snd

peelRun
  :: SchemaCache
  -> UserInfo
  -> HTTP.Manager
  -> Q.PGPool -> Q.TxIsolation
  -> Run a -> ExceptT QErr IO (a, SchemaCache)
peelRun sc userInfo httMgr pgPool txIso (Run m) =
  case lazyTx of
    LTErr e  -> throwError e
    LTNoTx a -> return a
    LTTx tx  -> Q.runTx pgPool (txIso, Nothing) $
                setHeadersTx (userVars userInfo) >> tx
  where
    lazyTx = runReaderT (runStateT m sc) (userInfo, httMgr)

runQuery
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo -> SchemaCache -> HTTP.Manager
  -> RQLQuery -> m (BL.ByteString, SchemaCache)
runQuery pool isoL userInfo sc hMgr query = do
  res <- liftIO $ runExceptT $
         peelRun sc userInfo hMgr pool isoL $ runQueryM query
  liftEither res

queryNeedsReload :: RQLQuery -> Bool
queryNeedsReload qi = case qi of
  RQAddExistingTableOrView _   -> True
  RQTrackTable _               -> True
  RQUntrackTable _             -> True

  RQCreateObjectRelationship _ -> True
  RQCreateArrayRelationship  _ -> True
  RQDropRelationship  _        -> True
  RQSetRelationshipComment  _  -> False

  RQCreateInsertPermission _   -> True
  RQCreateSelectPermission _   -> True
  RQCreateUpdatePermission _   -> True
  RQCreateDeletePermission _   -> True

  RQDropInsertPermission _     -> True
  RQDropSelectPermission _     -> True
  RQDropUpdatePermission _     -> True
  RQDropDeletePermission _     -> True
  RQSetPermissionComment _     -> False

  RQInsert _                   -> False
  RQSelect _                   -> False
  RQUpdate _                   -> False
  RQDelete _                   -> False
  RQCount _                    -> False

  RQAddRemoteSchema _          -> True
  RQRemoveRemoteSchema _       -> True

  RQCreateEventTrigger _       -> True
  RQDeleteEventTrigger _       -> True
  RQDeliverEvent _             -> False

  RQCreateQueryTemplate _      -> True
  RQDropQueryTemplate _        -> True
  RQExecuteQueryTemplate _     -> False
  RQSetQueryTemplateComment _  -> False

  RQRunSql _                   -> True

  RQReplaceMetadata _          -> True
  RQExportMetadata _           -> False
  RQClearMetadata _            -> True
  RQReloadMetadata _           -> True

  RQDumpInternalState _        -> False

  RQBulk qs                    -> any queryNeedsReload qs

runQueryM
  :: ( QErrM m, CacheRWM m, UserInfoM m, MonadTx m
     , MonadIO m, HasHttpManager m
     )
  => RQLQuery
  -> m RespBody
runQueryM rq = case rq of
  RQAddExistingTableOrView q -> runTrackTableQ q
  RQTrackTable q             -> runTrackTableQ q
  RQUntrackTable q           -> runUntrackTableQ q

  RQCreateObjectRelationship q -> runCreateObjRel q
  RQCreateArrayRelationship  q -> runCreateArrRel q
  RQDropRelationship  q        -> runDropRel q
  RQSetRelationshipComment  q  -> runSetRelComment q

  RQCreateInsertPermission q -> runCreatePerm q
  RQCreateSelectPermission q -> runCreatePerm q
  RQCreateUpdatePermission q -> runCreatePerm q
  RQCreateDeletePermission q -> runCreatePerm q

  RQDropInsertPermission q -> runDropPerm q
  RQDropSelectPermission q -> runDropPerm q
  RQDropUpdatePermission q -> runDropPerm q
  RQDropDeletePermission q -> runDropPerm q
  RQSetPermissionComment q -> runSetPermComment q

  RQInsert q -> runInsert q
  RQSelect q -> runSelect q
  RQUpdate q -> runUpdate q
  RQDelete q -> runDelete q
  RQCount  q -> runCount q

  RQAddRemoteSchema    q -> runAddRemoteSchema q
  RQRemoveRemoteSchema q -> runRemoveRemoteSchema q

  RQCreateEventTrigger q -> runCreateEventTriggerQuery q
  RQDeleteEventTrigger q -> runDeleteEventTriggerQuery q
  RQDeliverEvent q       -> runDeliverEvent q

  RQCreateQueryTemplate q     -> runCreateQueryTemplate q
  RQDropQueryTemplate q       -> runDropQueryTemplate q
  RQExecuteQueryTemplate q    -> runExecQueryTemplate q
  RQSetQueryTemplateComment q -> runSetQueryTemplateComment q

  RQReplaceMetadata q -> runReplaceMetadata q
  RQClearMetadata q   -> runClearMetadata q
  RQExportMetadata q  -> runExportMetadata q
  RQReloadMetadata q  -> runReloadMetadata q

  RQDumpInternalState q -> runDumpInternalState q

  RQRunSql q -> runRunSQL q

  RQBulk qs ->
    withPathK "args" $ do
    respList <- indexedMapM runQueryM qs
    let bsVector = V.fromList respList
    return $ BB.toLazyByteString $ encodeJSONVector BB.lazyByteString bsVector

setHeadersTx :: UserVars -> Q.TxE QErr ()
setHeadersTx uVars =
  Q.unitQE defaultTxErrorHandler setSess () False
  where
    toStrictText = LT.toStrict . AT.encodeToLazyText
    setSess = Q.fromText $
      "SET LOCAL \"hasura.user\" = " <>
      pgFmtLit (toStrictText uVars)
