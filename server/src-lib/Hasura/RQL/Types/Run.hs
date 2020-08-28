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
  { _rcUserInfo     :: !UserInfo
  , _rcHttpMgr      :: !HTTP.Manager
  , _rcSqlGenCtx    :: !SQLGenCtx
  , _rcMetadataPool :: !Q.PGPool
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

  runTxInMetadataStorage tx = do
    metadataPool <- asks _rcMetadataPool
    either throwError pure
      =<< (liftIO . runExceptT . Q.runTx metadataPool (Q.Serializable, Nothing)) tx

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
  let runCtx = RunCtx userInfo httpManager sqlGenCtx metadataPGPool
  metadata <- fromMaybe emptyMetadata <$> runMetadataStorageTx metadataFetchTx
  (r, updatedMetadata) <- runLazyTx pgExecCtx txAccess $
    maybe id withTraceContext ctx $ withUserInfo userInfo $ runStateT (runReaderT m runCtx) metadata
  either throwError pure =<<
    (liftIO . runExceptT . Q.runTx' metadataPGPool) (metadataUpdateTx updatedMetadata)
  pure r
  where
    runMetadataStorageTx tx =
      either throwError pure =<< (liftIO . runExceptT . Q.runTx' metadataPGPool) tx
