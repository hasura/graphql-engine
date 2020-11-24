module Hasura.RQL.Types
  ( MonadTx(..)

  , QCtx(..)
  , HasQCtx(..)
  , mkAdminQCtx
  , askTabInfo
  , isTableTracked
  , getTableInfo
  , askTableCoreInfo
  , askFieldInfoMap
  , askPGType
  , assertPGCol
  , askRelType
  , askFieldInfo
  , askPGColInfo
  , askComputedFieldInfo
  , askRemoteRel
  , askCurRole
  , askEventTriggerInfo
  , askTabInfoFromTrigger

  , HeaderObj

  , liftMaybe
  , module R
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M

import           Data.Text.Extended

import           Hasura.Backends.Postgres.Connection as R
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.RQL.IR.BoolExp               as R
import           Hasura.RQL.Types.Action             as R
import           Hasura.RQL.Types.Class              as R
import           Hasura.RQL.Types.Column             as R
import           Hasura.RQL.Types.Common             as R
import           Hasura.RQL.Types.ComputedField      as R
import           Hasura.RQL.Types.CustomTypes        as R
import           Hasura.RQL.Types.Error              as R
import           Hasura.RQL.Types.EventTrigger       as R
import           Hasura.RQL.Types.Function           as R
import           Hasura.RQL.Types.Metadata           as R
import           Hasura.RQL.Types.Permission         as R
import           Hasura.RQL.Types.QueryCollection    as R
import           Hasura.RQL.Types.Relationship       as R
import           Hasura.RQL.Types.RemoteRelationship as R
import           Hasura.RQL.Types.RemoteSchema       as R
import           Hasura.RQL.Types.ScheduledTrigger   as R
import           Hasura.RQL.Types.SchemaCache        as R
import           Hasura.RQL.Types.SchemaCache.Build  as R
import           Hasura.RQL.Types.Table              as R
import           Hasura.Session
import           Hasura.SQL.Backend                  as R

data QCtx
  = QCtx
  { qcUserInfo    :: !UserInfo
  , qcSchemaCache :: !SchemaCache
  , qcSQLCtx      :: !SQLGenCtx
  }

class HasQCtx a where
  getQCtx :: a -> QCtx

instance HasQCtx QCtx where
  getQCtx = id

mkAdminQCtx :: SQLGenCtx -> SchemaCache ->  QCtx
mkAdminQCtx soc sc = QCtx adminUserInfo sc soc

askTabInfo
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> m (TableInfo 'Postgres)
askTabInfo tabName = do
  rawSchemaCache <- askSchemaCache
  liftMaybe (err400 NotExists errMsg) $ M.lookup tabName $ scTables rawSchemaCache
  where
    errMsg = "table " <> tabName <<> " does not exist"

isTableTracked :: SchemaCache -> QualifiedTable -> Bool
isTableTracked sc qt =
  isJust $ M.lookup qt $ scTables sc

askTabInfoFromTrigger
  :: (QErrM m, CacheRM m)
  => TriggerName -> m (TableInfo 'Postgres)
askTabInfoFromTrigger trn = do
  sc <- askSchemaCache
  let tabInfos = M.elems $ scTables sc
  liftMaybe (err400 NotExists errMsg) $ find (isJust.M.lookup trn._tiEventTriggerInfoMap) tabInfos
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"

askEventTriggerInfo
  :: (QErrM m, CacheRM m)
  => TriggerName -> m EventTriggerInfo
askEventTriggerInfo trn = do
  ti <- askTabInfoFromTrigger trn
  let etim = _tiEventTriggerInfoMap ti
  liftMaybe (err400 NotExists errMsg) $ M.lookup trn etim
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"

liftMaybe :: (QErrM m) => QErr -> Maybe a -> m a
liftMaybe e = maybe (throwError e) return

throwTableDoesNotExist :: (QErrM m) => QualifiedTable -> m a
throwTableDoesNotExist tableName = throw400 NotExists ("table " <> tableName <<> " does not exist")

getTableInfo :: (QErrM m) => QualifiedTable -> HashMap QualifiedTable a -> m a
getTableInfo tableName infoMap =
  M.lookup tableName infoMap `onNothing` throwTableDoesNotExist tableName

askTableCoreInfo :: (QErrM m, TableCoreInfoRM m) => QualifiedTable -> m (TableCoreInfo 'Postgres)
askTableCoreInfo tableName =
  lookupTableCoreInfo tableName >>= (`onNothing` throwTableDoesNotExist tableName)

askFieldInfoMap :: (QErrM m, TableCoreInfoRM m) => QualifiedTable -> m (FieldInfoMap (FieldInfo 'Postgres))
askFieldInfoMap = fmap _tciFieldInfoMap . askTableCoreInfo

askPGType
  :: (MonadError QErr m)
  => FieldInfoMap (FieldInfo 'Postgres)
  -> PGCol
  -> Text
  -> m PGColumnType
askPGType m c msg =
  pgiType <$> askPGColInfo m c msg

askPGColInfo
  :: (MonadError QErr m)
  => FieldInfoMap (FieldInfo backend)
  -> PGCol
  -> Text
  -> m (ColumnInfo backend)
askPGColInfo m c msg = do
  fieldInfo <- modifyErr ("column " <>) $
             askFieldInfo m (fromPGCol c)
  case fieldInfo of
    (FIColumn pgColInfo)     -> pure pgColInfo
    (FIRelationship   _)     -> throwErr "relationship"
    (FIComputedField _)      -> throwErr "computed field"
    (FIRemoteRelationship _) -> throwErr "remote relationship"
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a postgres column; but, "
      , c <<> " is a " <> fieldType <> "; "
      , msg
      ]

askComputedFieldInfo
  :: (MonadError QErr m)
  => FieldInfoMap (FieldInfo backend)
  -> ComputedFieldName
  -> m (ComputedFieldInfo backend)
askComputedFieldInfo fields computedField = do
  fieldInfo <- modifyErr ("computed field " <>) $
               askFieldInfo fields $ fromComputedField computedField
  case fieldInfo of
    (FIColumn           _)       -> throwErr "column"
    (FIRelationship     _)       -> throwErr "relationship"
    (FIRemoteRelationship     _) -> throwErr "remote relationship"
    (FIComputedField cci)        -> pure cci
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a computed field; but, "
      , computedField <<> " is a " <> fieldType <> "; "
      ]

assertPGCol :: (MonadError QErr m)
            => FieldInfoMap (FieldInfo backend)
            -> Text
            -> PGCol
            -> m ()
assertPGCol m msg c = do
  _ <- askPGColInfo m c msg
  return ()

askRelType :: (MonadError QErr m)
           => FieldInfoMap (FieldInfo backend)
           -> RelName
           -> Text
           -> m RelInfo
askRelType m r msg = do
  colInfo <- modifyErr ("relationship " <>) $
             askFieldInfo m (fromRel r)
  case colInfo of
    (FIRelationship relInfo) -> return relInfo
    _                        ->
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a relationship; but, "
      , r <<> " is a postgres column; "
      , msg
      ]

askFieldInfo :: (MonadError QErr m)
             => FieldInfoMap fieldInfo
             -> FieldName
             -> m fieldInfo
askFieldInfo m f =
  onNothing (M.lookup f m) $
    throw400 NotExists $ mconcat
    [ f <<> " does not exist"
    ]

askRemoteRel :: (MonadError QErr m)
           => FieldInfoMap (FieldInfo backend)
           -> RemoteRelationshipName
           -> m (RemoteFieldInfo backend)
askRemoteRel fieldInfoMap relName = do
  fieldInfo <- askFieldInfo fieldInfoMap (fromRemoteRelationship relName)
  case fieldInfo of
    (FIRemoteRelationship remoteFieldInfo) -> return remoteFieldInfo
    _                                      ->
      throw400 UnexpectedPayload "expecting a remote relationship"

askCurRole :: (UserInfoM m) => m RoleName
askCurRole = _uiRole <$> askUserInfo

type HeaderObj = M.HashMap Text Text
