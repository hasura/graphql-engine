{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( RunT (..),
    RunCtx (..),
    peelRun,
  )
where

import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.EventTrigger (MonadEventLogCleanup (..))
import Hasura.RQL.Types.Source
import Hasura.Server.Types
import Hasura.Services
import Hasura.Session
import Hasura.Tracing qualified as Tracing

data RunCtx = RunCtx
  { _rcUserInfo :: UserInfo,
    _rcServerConfigCtx :: ServerConfigCtx
  }

newtype RunT m a = RunT {unRunT :: ReaderT RunCtx m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader RunCtx,
      MonadError e,
      MonadIO,
      Tracing.MonadTrace,
      MonadBase b,
      MonadBaseControl b,
      MonadMetadataStorage,
      MonadMetadataStorageQueryAPI,
      ProvidesNetwork
    )

instance MonadTrans RunT where
  lift = RunT . lift

instance (Monad m) => UserInfoM (RunT m) where
  askUserInfo = asks _rcUserInfo

instance (Monad m) => HasServerConfigCtx (RunT m) where
  askServerConfigCtx = asks _rcServerConfigCtx

instance (MonadResolveSource m) => MonadResolveSource (RunT m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

instance (MonadEventLogCleanup m) => MonadEventLogCleanup (RunT m) where
  runLogCleaner conf = lift $ runLogCleaner conf
  generateCleanupSchedules sInfo tName cConf = lift $ generateCleanupSchedules sInfo tName cConf
  updateTriggerCleanupSchedules logger oldSources newSources schemaCache = lift $ updateTriggerCleanupSchedules logger oldSources newSources schemaCache

peelRun ::
  RunCtx ->
  RunT m a ->
  m a
peelRun runCtx (RunT m) = runReaderT m runCtx
