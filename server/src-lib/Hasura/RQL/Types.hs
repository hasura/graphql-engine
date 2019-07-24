{-# LANGUAGE ConstraintKinds #-}

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

       , QCtx(..)
       , HasQCtx(..)
       , mkAdminQCtx

       , askTabInfo
       , askFieldInfoMap
       , askPGType
       , assertPGCol
       , askRelType
       , askFieldInfo
       , askPGColInfo
       , askCurRole
       , askEventTriggerInfo
       , askTabInfoFromTrigger
       , askRemoteRel
       , assertRemoteRel

       , adminOnly

       , HeaderObj

       , liftMaybe
       , module R
       ) where

import           Hasura.Db                           as R
import           Hasura.EncJSON
import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp            as R
import           Hasura.RQL.Types.Common             as R
import           Hasura.RQL.Types.DML                as R
import           Hasura.RQL.Types.Error              as R
import           Hasura.RQL.Types.EventTrigger       as R
import           Hasura.RQL.Types.Metadata           as R
import           Hasura.RQL.Types.Permission         as R
import           Hasura.RQL.Types.RemoteRelationship as R
import           Hasura.RQL.Types.RemoteSchema       as R
import           Hasura.RQL.Types.SchemaCache        as R

import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Context              as GC

import qualified Data.HashMap.Strict                 as M
import qualified Data.Text                           as T
import qualified Network.HTTP.Client                 as HTTP


getFieldInfoMap
  :: QualifiedTable
  -> SchemaCache -> Maybe FieldInfoMap
getFieldInfoMap tn =
  fmap tiFieldInfoMap . M.lookup tn . scTables

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

askTabInfo
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> m TableInfo
askTabInfo tabName = do
  rawSchemaCache <- askSchemaCache
  liftMaybe (err400 NotExists errMsg) $ M.lookup tabName $ scTables rawSchemaCache
  where
    errMsg = "table " <> tabName <<> " does not exist"

askTabInfoFromTrigger
  :: (QErrM m, CacheRM m)
  => TriggerName -> m TableInfo
askTabInfoFromTrigger trn = do
  sc <- askSchemaCache
  let tabInfos = M.elems $ scTables sc
  liftMaybe (err400 NotExists errMsg) $ find (isJust.M.lookup trn.tiEventTriggerInfoMap) tabInfos
  where
    errMsg = "event trigger " <> triggerNameToTxt trn <<> " does not exist"

askEventTriggerInfo
  :: (QErrM m, CacheRM m)
  => TriggerName -> m EventTriggerInfo
askEventTriggerInfo trn = do
  ti <- askTabInfoFromTrigger trn
  let etim = tiEventTriggerInfoMap ti
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

class (Monad m) => HasGCtxMap m where
  askGCtxMap :: m GC.GCtxMap

newtype SQLGenCtx
  = SQLGenCtx
  { stringifyNum :: Bool
  } deriving (Show, Eq)

class (Monad m) => HasSQLGenCtx m where
  askSQLGenCtx :: m SQLGenCtx

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
  => QualifiedTable -> m FieldInfoMap
askFieldInfoMap tabName = do
  mFieldInfoMap <- getFieldInfoMap tabName <$> askSchemaCache
  maybe (throw400 NotExists errMsg) return mFieldInfoMap
  where
    errMsg = "table " <> tabName <<> " does not exist"

askPGType
  :: (MonadError QErr m)
  => FieldInfoMap
  -> PGCol
  -> T.Text
  -> m PGColType
askPGType m c msg =
  pgiType <$> askPGColInfo m c msg

askPGColInfo
  :: (MonadError QErr m)
  => FieldInfoMap
  -> PGCol
  -> T.Text
  -> m PGColInfo
askPGColInfo m c msg = do
  colInfo <- modifyErr ("column " <>) $
             askFieldInfo m (fromPGCol c)
  case colInfo of
    (FIColumn pgColInfo) ->
      return pgColInfo
    _                      ->
      throwError $ err400 UnexpectedPayload $ mconcat
      [ "expecting a postgres column; but, "
      , c <<> " is a relationship; "
      , msg
      ]

assertPGCol :: (MonadError QErr m)
            => FieldInfoMap
            -> T.Text
            -> PGCol
            -> m ()
assertPGCol m msg c = do
  _ <- askPGType m c msg
  return ()

askRelType :: (MonadError QErr m)
           => FieldInfoMap
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
           => FieldInfoMap
           -> FieldName
           -> m FieldInfo
askFieldInfo m f =
  case M.lookup f m of
  Just colInfo -> return colInfo
  Nothing ->
    throw400 NotExists $ mconcat
    [ f <<> " does not exist"
    ]

assertRemoteRel :: (MonadError QErr m)
            => FieldInfoMap
            -> RemoteRelationshipName
            -> m ()
assertRemoteRel fieldInfoMap relName = do
  _ <- askRemoteRel fieldInfoMap relName
  return ()

askRemoteRel :: (MonadError QErr m)
           => FieldInfoMap
           -> RemoteRelationshipName
           -> m RemoteField
askRemoteRel fieldInfoMap relName = do
  fieldInfo <- askFieldInfo fieldInfoMap (FieldName (unRemoteRelationshipName relName))
  case fieldInfo of
    (FIRemote remoteField) -> return remoteField
    _                        ->
      throwError $ err400 UnexpectedPayload "expecting a remote relationship"

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
