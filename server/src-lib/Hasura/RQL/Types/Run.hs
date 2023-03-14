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
import Hasura.RQL.DDL.ApiLimit (MonadGetApiTimeLimit (..))
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

newtype RunT m a = RunT {unRunT :: RunCtx -> m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadIO,
      Tracing.MonadTrace,
      MonadBase b,
      MonadBaseControl b,
      MonadMetadataStorage,
      MonadMetadataStorageQueryAPI,
      ProvidesNetwork,
      MonadResolveSource,
      MonadEventLogCleanup,
      MonadGetApiTimeLimit
    )
    via (ReaderT RunCtx m)
  deriving (MonadTrans) via (ReaderT RunCtx)

instance (Monad m) => UserInfoM (RunT m) where
  askUserInfo = RunT $ pure . _rcUserInfo

instance (Monad m) => HasServerConfigCtx (RunT m) where
  askServerConfigCtx = RunT $ pure . _rcServerConfigCtx

peelRun ::
  RunCtx ->
  RunT m a ->
  m a
peelRun = flip unRunT
