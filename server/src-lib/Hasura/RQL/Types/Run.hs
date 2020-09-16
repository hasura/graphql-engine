{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( MetadataRun(..)
  , RunCtx(..)
  , peelMetadataRun
  , QueryRun(..)
  , peelQueryRun
  ) where

import           Hasura.Prelude
import           Hasura.Session

import qualified Database.PG.Query           as Q
import qualified Network.HTTP.Client         as HTTP

import           Control.Monad.Morph         (hoist)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Unique

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

newtype MetadataRun a
  = MetadataRun {unRun :: ReaderStateT (ExceptT QErr IO) a}
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           , MonadState Metadata
           )

instance UserInfoM MetadataRun where
  askUserInfo = asks _rcUserInfo

instance HasHttpManager MetadataRun where
  askHttpManager = asks _rcHttpMgr

instance HasSQLGenCtx MetadataRun where
  askSQLGenCtx = asks _rcSqlGenCtx

instance HasDefaultSource MetadataRun where
  askDefaultSource = asks _rcDefaultPgSource

instance MonadMetadata MetadataRun where
  fetchMetadata = get

  updateMetadata = put

peelMetadataRun
  :: (MonadIO m)
  => RunCtx
  -> Metadata
  -> MetadataRun a
  -> ExceptT QErr m (a, Metadata)
peelMetadataRun runCtx metadata (MetadataRun m) =
  hoist liftIO $ runStateT (runReaderT m runCtx) metadata

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
