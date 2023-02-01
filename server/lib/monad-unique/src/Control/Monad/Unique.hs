-- | A tiny mtl-style wrapper around 'U.newUnique'.
module Control.Monad.Unique
  ( U.Unique,
    MonadUnique (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Strict (WriterT)
import Data.Unique qualified as U

class (Monad m) => MonadUnique m where
  newUnique :: m U.Unique

instance MonadUnique IO where
  newUnique = U.newUnique

instance (MonadUnique m) => MonadUnique (ExceptT e m) where
  newUnique = lift newUnique

instance (MonadUnique m) => MonadUnique (ReaderT r m) where
  newUnique = lift newUnique

instance (MonadUnique m) => MonadUnique (StateT s m) where
  newUnique = lift newUnique

instance (Monoid w, MonadUnique m) => MonadUnique (WriterT w m) where
  newUnique = lift newUnique
