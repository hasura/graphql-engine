module Network.HTTP.Client.Extended
  ( HasHttpManagerM(..)
  , module HTTP
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Network.HTTP.Client         as HTTP


class (Monad m) => HasHttpManagerM m where
  askHttpManager :: m HTTP.Manager

instance (HasHttpManagerM m) => HasHttpManagerM (ExceptT e m) where
  askHttpManager = lift askHttpManager
instance (HasHttpManagerM m) => HasHttpManagerM (ReaderT r m) where
  askHttpManager = lift askHttpManager
instance (HasHttpManagerM m) => HasHttpManagerM (StateT s m) where
  askHttpManager = lift askHttpManager
instance (Monoid w, HasHttpManagerM m) => HasHttpManagerM (WriterT w m) where
  askHttpManager = lift askHttpManager
