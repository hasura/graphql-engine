{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( RunT(..)
  , RunCtx(..)
  , runQueryLazyTx
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
  = RunT { unRunT :: ReaderT RunCtx (ExceptT QErr m) a }
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadMetadataStorage
           , Tracing.MonadTrace
           )

instance (MonadIO m) => MonadUnique (RunT m) where
  newUnique = liftIO newUnique

instance (MonadMetadataStorage m) => MonadMetadataStorageQueryAPI (RunT m)

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

instance (MonadResolveSource m) => MonadResolveSource (RunT m) where
  getSourceResolver = RunT . lift . lift $ getSourceResolver

runQueryLazyTx
  :: ( MonadIO m
     , MonadBaseControl IO m
     , MonadError QErr m
     , Tracing.MonadTrace m
     , UserInfoM m
     )
  => PGExecCtx
  -> Q.TxAccess
  -> LazyTxT QErr m a
  -> m a
runQueryLazyTx pgExecCtx txAccess tx = do
  traceCtx <- Tracing.currentContext
  userInfo <- askUserInfo
  liftEitherM
    $ runExceptT
    $ runLazyTx pgExecCtx txAccess
    $ withTraceContext traceCtx
    $ withUserInfo userInfo tx

peelRun
  :: RunCtx -> RunT m a -> ExceptT QErr m a
peelRun runCtx (RunT m) = runReaderT m runCtx
