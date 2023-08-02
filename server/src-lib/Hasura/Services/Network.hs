-- | Network service provider.
--
-- This module defines a Service (see Note [Services]) that provides access to
-- the network; for now, that only means providing a HTTP Manager. This is
-- consequentlt a simple analogue to `(MonadReader r m, Has Manager r)`, but
-- could be updated to either encompass other network utilities, or to provide a
-- more restricted interface if deemed useful. Alternatively this could be
-- removed altogether if all network calls were to be hidden behind more
-- specific services.
module Hasura.Services.Network
  ( ProvidesNetwork (..),
  )
where

import Hasura.Prelude
import Hasura.Tracing
import Network.HTTP.Client qualified as HTTP

--------------------------------------------------------------------------------

class (Monad m) => ProvidesNetwork m where
  askHTTPManager :: m HTTP.Manager

instance (ProvidesNetwork m) => ProvidesNetwork (ReaderT r m) where
  askHTTPManager = lift askHTTPManager

instance (Monoid w, ProvidesNetwork m) => ProvidesNetwork (WriterT w m) where
  askHTTPManager = lift askHTTPManager

instance (ProvidesNetwork m) => ProvidesNetwork (StateT s m) where
  askHTTPManager = lift askHTTPManager

instance (ProvidesNetwork m) => ProvidesNetwork (ExceptT e m) where
  askHTTPManager = lift askHTTPManager

instance (ProvidesNetwork m) => ProvidesNetwork (TraceT m) where
  askHTTPManager = lift askHTTPManager
