{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( BaseRunT(..)
  , runBaseRunT
  , MetadataRun(..)
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

import           Control.Monad.Morph         (hoist)
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

newtype BaseRunT m a
  = BaseRunT {unBaseRunT :: ReaderT RunCtx (StateT Metadata m) a}
  deriving ( Functor, Applicative, Monad
           , MonadReader RunCtx
           , MonadIO
           , MonadUnique
           , MonadState Metadata
           )
deriving instance (MonadError QErr m) => MonadError QErr (BaseRunT m)
deriving instance (MonadTx m) => MonadTx (BaseRunT m)
deriving instance (MonadBase IO m) => MonadBase IO (BaseRunT m)
deriving instance (MonadBaseControl IO m) => MonadBaseControl IO (BaseRunT m)

instance (Monad m) => UserInfoM (BaseRunT m) where
  askUserInfo = asks _rcUserInfo

instance (Monad m) => HasHttpManager (BaseRunT m) where
  askHttpManager = asks _rcHttpMgr

instance (Monad m) => HasSQLGenCtx (BaseRunT m) where
  askSQLGenCtx = asks _rcSqlGenCtx

instance (Monad m) => HasDefaultSource (BaseRunT m) where
  askDefaultSource = asks _rcDefaultPgSource

instance (Monad m) => MonadMetadata (BaseRunT m) where
  fetchMetadata = get
  updateMetadata = put

runBaseRunT :: RunCtx -> Metadata -> BaseRunT m a -> m (a, Metadata)
runBaseRunT runCtx metadata (BaseRunT m) =
  runStateT (runReaderT m runCtx) metadata

newtype MetadataRun m a
  = MetadataRun {unMetadataRun :: BaseRunT (ExceptT QErr m) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadUnique
           , MonadMetadata
           , UserInfoM
           , HasHttpManager
           , HasSQLGenCtx
           , HasDefaultSource
           )

runInMetadataRun :: (Monad m) => MetadataStorageT m a -> MetadataRun m a
runInMetadataRun =
  MetadataRun . BaseRunT . lift . lift . ExceptT . runMetadataStorageT

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
  runBaseRunT runCtx metadata m

newtype QueryRun a
  = QueryRun {unQueryRun :: BaseRunT (LazyTxT QErr IO) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadTx
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           , MonadMetadata
           , UserInfoM
           , HasHttpManager
           , HasSQLGenCtx
           , HasDefaultSource
           )

peelQueryRun
  :: (MonadIO m)
  => PGSourceConfig
  -> Q.TxAccess
  -> Maybe Tracing.TraceContext
  -> RunCtx
  -> Metadata
  -> QueryRun a -> ExceptT QErr m (a, Metadata)
peelQueryRun pgSourceConfig accessMode maybeTraceCtx runCtx metadata (QueryRun m) =
  runBaseRunT runCtx metadata m
  & runLazyTx (_pscExecCtx pgSourceConfig) accessMode
    . withUserInfo userInfo
    . maybe id withTraceContext maybeTraceCtx
  & hoist liftIO
  where
    userInfo = _rcUserInfo runCtx
