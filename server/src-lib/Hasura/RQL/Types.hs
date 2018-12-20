{-# LANGUAGE ConstraintKinds #-}

module Hasura.RQL.Types
       ( P1
       , liftP1
       , liftP1WithQCtx
       , MonadTx(..)

       , LazyTx
       , runLazyTx
       , withUserInfo

       , UserInfoM(..)
       , RespBody
       , successMsg

       , HasHttpManager (..)
       , HasGCtxMap (..)

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

       , askQTemplateInfo

       , adminOnly
       , defaultTxErrorHandler

       , HeaderObj

       , liftMaybe
       , module R
       ) where

import           Hasura.Prelude
import           Hasura.RQL.Types.BoolExp      as R
import           Hasura.RQL.Types.Common       as R
import           Hasura.RQL.Types.DML          as R
import           Hasura.RQL.Types.Error        as R
import           Hasura.RQL.Types.Permission   as R
import           Hasura.RQL.Types.RemoteSchema as R
import           Hasura.RQL.Types.SchemaCache  as R
import           Hasura.RQL.Types.Subscribe    as R
import           Hasura.SQL.Types

import qualified Hasura.GraphQL.Context        as GC

import qualified Database.PG.Query             as Q

import           Data.Aeson

import qualified Data.Aeson.Text               as AT
import qualified Data.ByteString.Lazy          as BL
import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Network.HTTP.Client           as HTTP

getFieldInfoMap
  :: QualifiedTable
  -> SchemaCache -> Maybe FieldInfoMap
getFieldInfoMap tn =
  fmap tiFieldInfoMap . M.lookup tn . scTables

type RespBody = BL.ByteString

data QCtx
  = QCtx
  { qcUserInfo    :: !UserInfo
  , qcSchemaCache :: !SchemaCache
  } deriving (Show, Eq)

class HasQCtx a where
  getQCtx :: a -> QCtx

instance HasQCtx QCtx where
  getQCtx = id

mkAdminQCtx :: SchemaCache -> QCtx
mkAdminQCtx = QCtx adminUserInfo

class (Monad m) => UserInfoM m where
  askUserInfo :: m UserInfo

type P1C m = (UserInfoM m, QErrM m, CacheRM m)

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
    errMsg = "event trigger " <> trn <<> " does not exist"

askEventTriggerInfo
  :: (QErrM m)
  => EventTriggerInfoMap -> TriggerName -> m EventTriggerInfo
askEventTriggerInfo etim trn = liftMaybe (err400 NotExists errMsg) $ M.lookup trn etim
  where
    errMsg = "event trigger " <> trn <<> " does not exist"

askQTemplateInfo
  :: (P1C m)
  => TQueryName
  -> m QueryTemplateInfo
askQTemplateInfo qtn = do
  rawSchemaCache <- askSchemaCache
  liftMaybe (err400 NotExists errMsg) $ M.lookup qtn $ scQTemplates rawSchemaCache
  where
    errMsg = "query-template " <> qtn <<> " does not exist"

instance UserInfoM P1 where
  askUserInfo = qcUserInfo <$> ask

instance CacheRM P1 where
  askSchemaCache = qcSchemaCache <$> ask

class (Monad m) => HasHttpManager m where
  askHttpManager :: m HTTP.Manager

class (Monad m) => HasGCtxMap m where
  askGCtxMap :: m GC.GCtxMap

class (MonadError QErr m) => MonadTx m where
  liftTx :: Q.TxE QErr a -> m a

instance (MonadTx m) => MonadTx (StateT s m) where
  liftTx = lift . liftTx

instance (MonadTx m) => MonadTx (ReaderT s m) where
  liftTx = lift . liftTx

data LazyTx e a
  = LTErr e
  | LTNoTx a
  | LTTx (Q.TxE e a)

lazyTxToQTx :: LazyTx e a -> Q.TxE e a
lazyTxToQTx = \case
  LTErr e  -> throwError e
  LTNoTx r -> return r
  LTTx tx  -> tx

runLazyTx
  :: Q.PGPool -> Q.TxIsolation
  -> LazyTx QErr a -> ExceptT QErr IO a
runLazyTx pgPool txIso = \case
  LTErr e  -> throwError e
  LTNoTx a -> return a
  LTTx tx  -> Q.runTx pgPool (txIso, Nothing) tx

setHeadersTx :: UserVars -> Q.TxE QErr ()
setHeadersTx uVars =
  Q.unitQE defaultTxErrorHandler setSess () False
  where
    toStrictText = LT.toStrict . AT.encodeToLazyText
    setSess = Q.fromText $
      "SET LOCAL \"hasura.user\" = " <>
      pgFmtLit (toStrictText uVars)

withUserInfo :: UserInfo -> LazyTx QErr a -> LazyTx QErr a
withUserInfo uInfo = \case
  LTErr e  -> LTErr e
  LTNoTx a -> LTNoTx a
  LTTx tx  -> LTTx $ setHeadersTx (userVars uInfo) >> tx

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

instance MonadTx (Q.TxE QErr) where
  liftTx = id

instance MonadIO (LazyTx QErr) where
  liftIO = LTTx . liftIO

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
     ) => P1 a -> m a
liftP1 m = do
  ui <- askUserInfo
  sc <- askSchemaCache
  let qCtx = QCtx ui sc
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

askCurRole :: (UserInfoM m) => m RoleName
askCurRole = userRole <$> askUserInfo

adminOnly :: (UserInfoM m, QErrM m) => m ()
adminOnly = do
  curRole <- askCurRole
  unless (curRole == adminRole) $ throw400 AccessDenied errMsg
  where
    errMsg = "restricted access : admin only"

defaultTxErrorHandler :: Q.PGTxErr -> QErr
defaultTxErrorHandler txe =
  let e = err500 PostgresError "postgres query error"
  in e {qeInternal = Just $ toJSON txe}

successMsg :: BL.ByteString
successMsg = "{\"message\":\"success\"}"

type HeaderObj = M.HashMap T.Text T.Text
