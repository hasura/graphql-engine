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

newtype Run a
  = Run { unRun :: ReaderT RunCtx (LazyTxT QErr IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadError QErr
           , MonadReader RunCtx
           , MonadTx
           , MonadIO
           , MonadBase IO
           , MonadBaseControl IO
           , MonadUnique
           )

instance UserInfoM Run where
  askUserInfo = asks _rcUserInfo

instance HasHttpManager Run where
  askHttpManager = asks _rcHttpMgr

instance HasSQLGenCtx Run where
  askSQLGenCtx = asks _rcSqlGenCtx

peelRun
  :: (MonadIO m)
  => RunCtx
  -> PGExecCtx
  -> Q.TxAccess
  -> Maybe Tracing.TraceContext
  -> Run a
  -> ExceptT QErr m a
peelRun runCtx@(RunCtx userInfo _ _) pgExecCtx txAccess ctx (Run m) =
  mapExceptT liftIO $ runLazyTx pgExecCtx txAccess $ maybe id withTraceContext ctx $ withUserInfo userInfo $ runReaderT m runCtx
