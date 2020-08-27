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

data MetadataManageCtx
  = MetadataManageCtx
  { _mmcPool     :: !Q.PGPool
  , _mmcFetchTx  :: !(Q.TxE QErr (Maybe Metadata))
  , _mmcUpdateTx :: !(Metadata -> Q.TxE QErr ())
  }

data RunCtx
  = RunCtx
  { _rcUserInfo  :: !UserInfo
  , _rcHttpMgr   :: !HTTP.Manager
  , _rcSqlGenCtx :: !SQLGenCtx
  , _rcMetadata  :: !MetadataManageCtx
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

instance MonadMetadata Run where
  fetchMetadata = do
    MetadataManageCtx{..} <- asks _rcMetadata
    fromMaybe emptyMetadata <$> runTxInMetadataStorage _mmcFetchTx

  updateMetadata metadata = do
    MetadataManageCtx{..} <- asks _rcMetadata
    runTxInMetadataStorage $ _mmcUpdateTx metadata

  runTxInMetadataStorage tx = do
    MetadataManageCtx{..} <- asks _rcMetadata
    either throwError pure
      =<< (liftIO . runExceptT . Q.runTx _mmcPool (Q.Serializable, Nothing)) tx

peelRun
  :: (MonadIO m, MonadMetadataManage m)
  => UserInfo
  -> HTTP.Manager
  -> SQLGenCtx
  -> Q.PGPool
  -> PGExecCtx
  -> Q.TxAccess
  -> Maybe Tracing.TraceContext
  -> Run a
  -> ExceptT QErr m a
peelRun userInfo httpManager sqlGenCtx metadataPGPool pgExecCtx txAccess ctx (Run m) = do
  metadataFetchTx <- lift fetchMetadataTx
  metadataUpdateTx <- lift updateMetadataTx
  let runCtx = RunCtx userInfo httpManager sqlGenCtx $
               MetadataManageCtx metadataPGPool metadataFetchTx metadataUpdateTx
  runLazyTx pgExecCtx txAccess $ maybe id withTraceContext ctx $ withUserInfo userInfo $ runReaderT m runCtx
