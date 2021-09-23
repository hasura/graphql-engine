module Control.Monad.Trans.Managed where

import           Prelude

import           Control.Exception.Lifted    (bracket, bracket_)
import           Control.Monad.Codensity     (Codensity (..))
import           Control.Monad.Fix           (MonadFix (..))
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader.Class  (MonadReader)
import           Control.Monad.State.Class   (MonadState)
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader  (ReaderT (..))
import           GHC.IO.Unsafe               (unsafeDupableInterleaveIO)

import qualified Control.Concurrent          as C

-- | This type is like a transformer version of the @Managed@ monad from the
-- @managed@ library. It can be used to manage resources by pairing together
-- their allocation with their finalizers.
--
-- The documentation for the @managed@ library is an excellent introduction to
-- the idea here.
--
-- We could use 'Codensity' directly, but we'd have to define an orphan instance
-- for 'MonadFix'. This also gives us the opportunity to give it a slightly more
-- friendly name.
--
-- We could also have used @ResourceT@, but that would have involved writing
-- instances for @MonadUnliftIO@. That could still be a good option to consider
-- later, however.
newtype ManagedT m a = ManagedT { runManagedT :: forall r. (a -> m r) -> m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader r
           , MonadState s
           ) via (Codensity m)
  deriving MonadTrans via Codensity

-- | Allocate a resource by providing setup and finalizer actions.
allocate :: MonadBaseControl IO m => m a -> (a -> m b) -> ManagedT m a
allocate setup finalize = ManagedT (bracket setup finalize)

-- | Allocate a resource but do not return a reference to it.
allocate_ :: MonadBaseControl IO m => m a -> m b -> ManagedT m ()
allocate_ setup finalize = ManagedT (\k -> bracket_ setup finalize (k ()))

-- | Run the provided computation by returning its result, and run any finalizers.
-- Watch out: this function might leak finalized resources.
lowerManagedT :: Monad m => ManagedT m a -> m a
lowerManagedT m = runManagedT m return

hoistManagedTReaderT :: Monad m => r -> ManagedT (ReaderT r m) a -> ManagedT m a
hoistManagedTReaderT r cod = ManagedT $ \k ->
  runReaderT (runManagedT cod (lift . k)) r

-- | We need this instance to tie the knot when initializing resources.
-- It'd be nice if we could do this with a 'MonadFix' constraint on the underlying
-- monad, but here we just use 'MonadIO' to tie the knot using a lazily-evaluated
-- 'MVar'-based promise for the eventual result.
--
-- We need to be careful not to leak allocated resources via the use of
-- recursively-defined monadic actions when making use of this instance.
instance MonadIO m => MonadFix (ManagedT m) where
  mfix f = ManagedT  \k -> do
    m <- liftIO C.newEmptyMVar
    ans <- liftIO $ unsafeDupableInterleaveIO (C.readMVar m)
    runManagedT (f ans) \a -> do
      liftIO $ C.putMVar m a
      k a
