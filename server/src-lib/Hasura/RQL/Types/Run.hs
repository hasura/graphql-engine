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

data RunCtx
  = RunCtx
  { _rcUserInfo  :: !UserInfo
  , _rcHttpMgr   :: !HTTP.Manager
  , _rcSqlGenCtx :: !SQLGenCtx
  }

newtype Run a
  = Run { unRun :: ReaderT RunCtx (LazyTx QErr) a }
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
  -> Run a
  -> ExceptT QErr m a
peelRun runCtx@(RunCtx userInfo _ _) pgExecCtx txAccess (Run m) =
  runLazyTx pgExecCtx txAccess $ withUserInfo userInfo $ runReaderT m runCtx
