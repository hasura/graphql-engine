{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( RunT(..)
  , RunCtx(..)
  , peelRun
  ) where

import           Hasura.Prelude
import           Hasura.Session

import qualified Database.PG.Query           as Q
import qualified Network.HTTP.Client         as HTTP

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Unique
import           Hasura.Metadata.Class

import           Hasura.RQL.Types
import qualified Hasura.Tracing              as Tracing

data RunCtx
  = RunCtx
  { _rcUserInfo             :: !UserInfo
  , _rcHttpMgr              :: !HTTP.Manager
  , _rcSqlGenCtx            :: !SQLGenCtx
  , _rcRemoteSchemaPermsCtx :: !RemoteSchemaPermsCtx
  }

newtype RunT m a
  = RunT { unRunT :: ReaderT RunCtx (LazyTxT QErr m) a }
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadTx
           , MonadIO
           , MonadUnique
           , MonadMetadataStorage
           )

instance (MonadMetadataStorage m) => MonadScheduledEvents (RunT m)

deriving instance (MonadIO m, MonadBase IO m) => MonadBase IO (RunT m)
deriving instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (RunT m)

instance (Monad m) => UserInfoM (RunT m) where
  askUserInfo = asks _rcUserInfo

instance (Monad m) => HasHttpManager (RunT m) where
  askHttpManager = asks _rcHttpMgr

instance (Monad m) => HasSQLGenCtx (RunT m) where
  askSQLGenCtx = asks _rcSqlGenCtx

instance (Monad m) => HasRemoteSchemaPermsCtx (RunT m) where
  askRemoteSchemaPermsCtx = asks _rcRemoteSchemaPermsCtx

peelRun
  :: ( MonadIO m
     , MonadBaseControl IO m
     )
  => RunCtx
  -> PGExecCtx
  -> Q.TxAccess
  -> Maybe Tracing.TraceContext
  -> RunT m a
  -> ExceptT QErr m a
peelRun runCtx pgExecCtx txAccess ctx (RunT m) =
  runLazyTx pgExecCtx txAccess $
  maybe id withTraceContext ctx $ withUserInfo userInfo $ runReaderT m runCtx
  where
    userInfo = _rcUserInfo runCtx
