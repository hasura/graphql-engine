{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.Run
  ( Run(..)
  , RunCtx(..)
  , peelRun
  ) where

import           Hasura.Prelude
import           Hasura.Session

import qualified Database.PG.Query           as Q
import qualified Network.HTTP.Client         as HTTP

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Unique

import           Hasura.RQL.Types
import qualified Hasura.Tracing              as Tracing

data RunCtx
  = RunCtx
  { _rcUserInfo  :: !UserInfo
  , _rcHttpMgr   :: !HTTP.Manager
  , _rcSqlGenCtx :: !SQLGenCtx
  }

-- TODO: Efficient way to handle multple postgres transactions (user db and metadata storage)
newtype Run a
  = Run { unRun :: ReaderT RunCtx (StateT Metadata (LazyTx QErr)) a }
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadTx
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           , MonadState Metadata
           )

instance UserInfoM Run where
  askUserInfo = asks _rcUserInfo

instance HasHttpManager Run where
  askHttpManager = asks _rcHttpMgr

instance HasSQLGenCtx Run where
  askSQLGenCtx = asks _rcSqlGenCtx

instance MonadMetadata Run where
  fetchMetadata = get

  updateMetadata = put

peelRun
  :: (MonadIO m)
  => RunCtx
  -> PGExecCtx
  -> Q.TxAccess
  -> Maybe Tracing.TraceContext
  -> Metadata
  -> Run a
  -> ExceptT QErr m (a, Metadata)
peelRun runCtx pgExecCtx txAccess ctx metadata (Run m) =
  runLazyTx pgExecCtx txAccess
  $ maybe id withTraceContext ctx
  $ withUserInfo (_rcUserInfo runCtx)
  $ runStateT (runReaderT m runCtx) metadata
