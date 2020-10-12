{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( MetadataRun(..)
  , RunCtx(..)
  , peelMetadataRun
  , QueryRun(..)
  , peelQueryRun
  ) where

import           Hasura.Class
import           Hasura.Prelude
import           Hasura.Session

import qualified Database.PG.Query           as Q
import qualified Network.HTTP.Client         as HTTP

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Unique
import           Data.Aeson

import           Hasura.RQL.Types
import qualified Hasura.Tracing              as Tracing

data RunCtx
  = RunCtx
  { _rcUserInfo        :: !UserInfo
  , _rcHttpMgr         :: !HTTP.Manager
  , _rcSqlGenCtx       :: !SQLGenCtx
  , _rcDefaultPgSource :: !PGSourceConfig
  }

type ReaderStateT m a = ReaderT RunCtx (StateT Metadata m) a

newtype MetadataRun m a
  = MetadataRun {unRun :: ReaderStateT (ExceptT QErr m) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadUnique
           , MonadState Metadata
           )

instance MonadTrans MetadataRun where
  lift = lift

instance (Monad m) => UserInfoM (MetadataRun m) where
  askUserInfo = asks _rcUserInfo

instance (Monad m) => HasHttpManager (MetadataRun m) where
  askHttpManager = asks _rcHttpMgr

instance (Monad m) => HasSQLGenCtx (MetadataRun m) where
  askSQLGenCtx = asks _rcSqlGenCtx

instance (Monad m) => HasDefaultSource (MetadataRun m) where
  askDefaultSource = asks _rcDefaultPgSource

instance (Monad m) => MonadMetadata (MetadataRun m) where
  fetchMetadata = get
  updateMetadata = put

runInMetadataRun :: (Monad m) => MetadataStorageT m a -> MetadataRun m a
runInMetadataRun =
  MetadataRun . lift . lift . ExceptT . runMetadataStorageT

instance (MonadMetadataStorage m) => MonadScheduledEvents (MetadataRun m) where
  createEvent                = runInMetadataRun . insertScheduledEvent
  dropFutureCronEvents       = runInMetadataRun . clearFutureCronEvents
  fetchInvocations a b       = runInMetadataRun $ getInvocations a b
  fetchScheduledEvents ev    = runInMetadataRun $ case _scheduledEvent ev of
    SEOneOff    -> toJSON <$> getOneOffScheduledEvents
    SECron name -> toJSON <$> getCronEvents name
  dropEvent a b              = runInMetadataRun $ deleteScheduledEvent a b

instance (MonadMetadataStorage m) => MonadCatalogState (MetadataRun m) where
  fetchCatalogState      = runInMetadataRun getCatalogState
  updateCatalogState a b = runInMetadataRun $ setCatalogState a b

peelMetadataRun
  :: RunCtx
  -> Metadata
  -> MetadataRun m a
  -> ExceptT QErr m (a, Metadata)
peelMetadataRun runCtx metadata (MetadataRun m) =
  runStateT (runReaderT m runCtx) metadata

newtype QueryRun a
  = QueryRun {unQueryRun :: ReaderStateT (LazyTx QErr) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadTx
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           , MonadState Metadata
           )

instance UserInfoM QueryRun where
  askUserInfo = asks _rcUserInfo

instance HasHttpManager QueryRun where
  askHttpManager = asks _rcHttpMgr

instance HasSQLGenCtx QueryRun where
  askSQLGenCtx = asks _rcSqlGenCtx

instance HasDefaultSource QueryRun where
  askDefaultSource = asks _rcDefaultPgSource

instance MonadMetadata QueryRun where
  fetchMetadata = get

  updateMetadata = put

peelQueryRun
  :: (MonadIO m)
  => PGSourceConfig
  -> Q.TxAccess
  -> Maybe Tracing.TraceContext
  -> RunCtx
  -> Metadata
  -> QueryRun a -> ExceptT QErr m (a, Metadata)
peelQueryRun pgSourceConfig accessMode maybeTraceCtx runCtx metadata (QueryRun m) =
  runStateT (runReaderT m runCtx) metadata
  & runLazyTx (_pscExecCtx pgSourceConfig) accessMode
    . withUserInfo userInfo
    . maybe id withTraceContext maybeTraceCtx
  where
    userInfo = _rcUserInfo runCtx
