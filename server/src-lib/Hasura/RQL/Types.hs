module Hasura.RQL.Types
       ( P1
       , liftP1
       , liftP1WithQCtx
       , MonadTx(..)

       , UserInfoM(..)
       , successMsg

       , HasHttpManager (..)
       , HasGCtxMap (..)

       , SQLGenCtx(..)
       , HasSQLGenCtx(..)

       , HasSystemDefined(..)
       , HasSystemDefinedT
       , runHasSystemDefinedT

       , QCtx(..)
       , HasQCtx(..)
       , mkAdminQCtx
       , askTabInfo
       , isTableTracked
       , askFieldInfoMap
       , askPGType
       , assertPGCol
       , askRelType
       , askFieldInfo
       , askPGColInfo
       , askComputedFieldInfo
       , askCurRole
       , askEventTriggerInfo
       , askTabInfoFromTrigger

       , updateComputedFieldFunctionDescription

       , adminOnly

       , HeaderObj

       , liftMaybe
       , module R
       ) where

import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.SQL.Types

import           Hasura.Db                      as R
import           Hasura.RQL.Types.BoolExp       as R
import           Hasura.RQL.Types.Column        as R
import           Hasura.RQL.Types.Common        as R
import           Hasura.RQL.Types.ComputedField as R
import           Hasura.RQL.Types.DML           as R
import           Hasura.RQL.Types.Error         as R
import           Hasura.RQL.Types.EventTrigger  as R
import           Hasura.RQL.Types.Metadata      as R
import           Hasura.RQL.Types.Permission    as R
import           Hasura.RQL.Types.RemoteSchema  as R
import           Hasura.RQL.Types.SchemaCache   as R

import qualified Hasura.GraphQL.Context         as GC

import qualified Data.HashMap.Strict            as M
import qualified Data.Text                      as T
import qualified Network.HTTP.Client            as HTTP

getFieldInfoMap
  :: QualifiedTable
  -> SchemaCache -> Maybe (FieldInfoMap PGColumnInfo)
getFieldInfoMap tn =
  fmap _tiFieldInfoMap . M.lookup tn . scTables

data QCtx
  = QCtx
  { qcUserInfo    :: !UserInfo
  , qcSchemaCache :: !SchemaCache
  , qcSQLCtx      :: !SQLGenCtx
  } deriving (Show, Eq)

class HasQCtx a where
  getQCtx :: a -> QCtx

instance HasQCtx QCtx where
  getQCtx = id

mkAdminQCtx :: SQLGenCtx -> SchemaCache ->  QCtx
mkAdminQCtx soc sc = QCtx adminUserInfo sc soc

class (Monad m) => UserInfoM m where
  askUserInfo :: m UserInfo

instance (UserInfoM m) => UserInfoM (ReaderT r m) where
  askUserInfo = lift askUserInfo

askTabInfo
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> m (TableInfo PGColumnInfo)
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
  => TriggerName -> m (TableInfo PGColumnInfo)
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

instance UserInfoM P1 where
  askUserInfo = qcUserInfo <$> ask

instance CacheRM P1 where
  askSchemaCache = qcSchemaCache <$> ask

instance HasSQLGenCtx P1 where
  askSQLGenCtx = qcSQLCtx <$> ask

class (Monad m) => HasHttpManager m where
  askHttpManager :: m HTTP.Manager

instance (HasHttpManager m) => HasHttpManager (ReaderT r m) where
  askHttpManager = lift askHttpManager

class (Monad m) => HasGCtxMap m where
  askGCtxMap :: m GC.GCtxMap

instance (HasGCtxMap m) => HasGCtxMap (ReaderT r m) where
  askGCtxMap = lift askGCtxMap

newtype SQLGenCtx
  = SQLGenCtx
  { stringifyNum :: Bool
  } deriving (Show, Eq)

class (Monad m) => HasSQLGenCtx m where
  askSQLGenCtx :: m SQLGenCtx

instance (HasSQLGenCtx m) => HasSQLGenCtx (ReaderT r m) where
  askSQLGenCtx = lift askSQLGenCtx

class (Monad m) => HasSystemDefined m where
  askSystemDefined :: m SystemDefined

instance (HasSystemDefined m) => HasSystemDefined (ReaderT r m) where
  askSystemDefined = lift askSystemDefined

newtype HasSystemDefinedT m a
  = HasSystemDefinedT { unHasSystemDefinedT :: ReaderT SystemDefined m a }
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadError e, MonadTx
           , HasHttpManager, HasSQLGenCtx, CacheRM, CacheRWM, UserInfoM )

runHasSystemDefinedT :: SystemDefined -> HasSystemDefinedT m a -> m a
runHasSystemDefinedT systemDefined = flip runReaderT systemDefined . unHasSystemDefinedT

instance (Monad m) => HasSystemDefined (HasSystemDefinedT m) where
  askSystemDefined = HasSystemDefinedT ask

type ER e r = ExceptT e (Reader r)
type P1 = ER QErr QCtx

runER :: r -> ER e r a -> Either e a
runER r m = runReader (runExceptT m) r

liftMaybe :: (QErrM m) => QErr -> Maybe a -> m a
liftMaybe e = maybe (throwError e) return

liftP1
  :: ( QErrM m
     , UserInfoM m
     , CacheRM m
     , HasSQLGenCtx m
     ) => P1 a -> m a
liftP1 m = do
  ui <- askUserInfo
  sc <- askSchemaCache
  sqlCtx <- askSQLGenCtx
  let qCtx = QCtx ui sc sqlCtx
  liftP1WithQCtx qCtx m

liftP1WithQCtx
  :: (MonadError e m) => r -> ER e r a -> m a
liftP1WithQCtx r m =
  liftEither $ runER r m

askFieldInfoMap
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> m (FieldInfoMap PGColumnInfo)
askFieldInfoMap tabName = do
  mFieldInfoMap <- getFieldInfoMap tabName <$> askSchemaCache
  maybe (throw400 NotExists errMsg) return mFieldInfoMap
  where
    errMsg = "table " <> tabName <<> " does not exist"

askPGType
  :: (MonadError QErr m)
  => FieldInfoMap PGColumnInfo
  -> PGCol
  -> T.Text
  -> m PGColumnType
askPGType m c msg =
  pgiType <$> askPGColInfo m c msg

askPGColInfo
  :: (MonadError QErr m)
  => FieldInfoMap columnInfo
  -> PGCol
  -> T.Text
  -> m columnInfo
askPGColInfo m c msg = do
  fieldInfo <- modifyErr ("column " <>) $
             askFieldInfo m (fromPGCol c)
  case fieldInfo of
    (FIColumn pgColInfo) -> pure pgColInfo
    (FIRelationship   _) -> throwErr "relationship"
    (FIComputedField _)  -> throwErr "computed field"
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a postgres column; but, "
      , c <<> " is a " <> fieldType <> "; "
      , msg
      ]

askComputedFieldInfo
  :: (MonadError QErr m)
  => FieldInfoMap columnInfo
  -> ComputedFieldName
  -> m ComputedFieldInfo
askComputedFieldInfo fields computedField = do
  fieldInfo <- modifyErr ("computed field " <>) $
               askFieldInfo fields $ fromComputedField computedField
  case fieldInfo of
    (FIColumn           _) -> throwErr "column"
    (FIRelationship     _) -> throwErr "relationship"
    (FIComputedField cci)  -> pure cci
  where
    throwErr fieldType =
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a computed field; but, "
      , computedField <<> " is a " <> fieldType <> "; "
      ]

updateComputedFieldFunctionDescription
  :: (QErrM m, CacheRWM m)
  => QualifiedTable -> ComputedFieldName -> Maybe PGDescription -> m ()
updateComputedFieldFunctionDescription table computedField description = do
  fields <- _tiFieldInfoMap <$> askTabInfo table
  computedFieldInfo <- askComputedFieldInfo fields computedField
  deleteComputedFieldFromCache table computedField
  let updatedComputedFieldInfo = computedFieldInfo
                                  { _cfiFunction = (_cfiFunction computedFieldInfo)
                                                   {_cffDescription = description}
                                  }
  addComputedFieldToCache table updatedComputedFieldInfo

assertPGCol :: (MonadError QErr m)
            => FieldInfoMap columnInfo
            -> T.Text
            -> PGCol
            -> m ()
assertPGCol m msg c = do
  _ <- askPGColInfo m c msg
  return ()

askRelType :: (MonadError QErr m)
           => FieldInfoMap columnInfo
           -> RelName
           -> T.Text
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
           => FieldInfoMap columnInfo
           -> FieldName
           -> m (FieldInfo columnInfo)
askFieldInfo m f =
  case M.lookup f m of
  Just colInfo -> return colInfo
  Nothing ->
    throw400 NotExists $ mconcat
    [ f <<> " does not exist"
    ]

askCurRole :: (UserInfoM m) => m RoleName
askCurRole = userRole <$> askUserInfo

adminOnly :: (UserInfoM m, QErrM m) => m ()
adminOnly = do
  curRole <- askCurRole
  unless (curRole == adminRole) $ throw400 AccessDenied errMsg
  where
    errMsg = "restricted access : admin only"

successMsg :: EncJSON
successMsg = "{\"message\":\"success\"}"

type HeaderObj = M.HashMap T.Text T.Text
