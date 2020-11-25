module Hasura.RQL.Types.Class where

import           Hasura.Backends.Postgres.Connection
import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.SchemaCache
import           Hasura.Session
import           Hasura.Tracing

import           Control.Monad.Unique

import qualified Network.HTTP.Client                 as HTTP

class (Monad m) => UserInfoM m where
  askUserInfo :: m UserInfo

instance (UserInfoM m) => UserInfoM (ReaderT r m) where
  askUserInfo = lift askUserInfo
instance (UserInfoM m) => UserInfoM (StateT s m) where
  askUserInfo = lift askUserInfo
instance (UserInfoM m) => UserInfoM (TraceT m) where
  askUserInfo = lift askUserInfo

class (Monad m) => HasHttpManager m where
  askHttpManager :: m HTTP.Manager

instance (HasHttpManager m) => HasHttpManager (ExceptT e m) where
  askHttpManager = lift askHttpManager
instance (HasHttpManager m) => HasHttpManager (ReaderT r m) where
  askHttpManager = lift askHttpManager
instance (HasHttpManager m) => HasHttpManager (StateT s m) where
  askHttpManager = lift askHttpManager
instance (Monoid w, HasHttpManager m) => HasHttpManager (WriterT w m) where
  askHttpManager = lift askHttpManager
instance (HasHttpManager m) => HasHttpManager (TraceT m) where
  askHttpManager = lift askHttpManager

class (Monad m) => HasSQLGenCtx m where
  askSQLGenCtx :: m SQLGenCtx

instance (HasSQLGenCtx m) => HasSQLGenCtx (ReaderT r m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (StateT s m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (Monoid w, HasSQLGenCtx m) => HasSQLGenCtx (WriterT w m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (TableCoreCacheRT m) where
  askSQLGenCtx = lift askSQLGenCtx
instance (HasSQLGenCtx m) => HasSQLGenCtx (TraceT m) where
  askSQLGenCtx = lift askSQLGenCtx

class (Monad m) => HasSystemDefined m where
  askSystemDefined :: m SystemDefined

instance (HasSystemDefined m) => HasSystemDefined (ReaderT r m) where
  askSystemDefined = lift askSystemDefined
instance (HasSystemDefined m) => HasSystemDefined (StateT s m) where
  askSystemDefined = lift askSystemDefined
instance (Monoid w, HasSystemDefined m) => HasSystemDefined (WriterT w m) where
  askSystemDefined = lift askSystemDefined
instance (HasSystemDefined m) => HasSystemDefined (TraceT m) where
  askSystemDefined = lift askSystemDefined

newtype HasSystemDefinedT m a
  = HasSystemDefinedT { unHasSystemDefinedT :: ReaderT SystemDefined m a }
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadUnique, MonadError e, MonadTx
           , HasHttpManager, HasSQLGenCtx, TableCoreInfoRM, CacheRM, UserInfoM)

runHasSystemDefinedT :: SystemDefined -> HasSystemDefinedT m a -> m a
runHasSystemDefinedT systemDefined = flip runReaderT systemDefined . unHasSystemDefinedT

instance (Monad m) => HasSystemDefined (HasSystemDefinedT m) where
  askSystemDefined = HasSystemDefinedT ask
