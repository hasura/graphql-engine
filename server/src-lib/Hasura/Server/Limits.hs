module Hasura.Server.Limits
  ( HasResourceLimits (..),
    ResourceLimits (..),
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types.ApiLimit (ApiLimit)
import Hasura.Server.Types qualified as HGE
import Hasura.Session (UserInfo)
import Hasura.Tracing qualified as Tracing

-- | Resource limits, represented by a function which modifies IO actions to
-- enforce those limits by throwing errors using 'MonadError' in the case
-- where they are exceeded.
data ResourceLimits = ResourceLimits
  { runResourceLimits ::
      forall m a.
      (MonadBaseControl IO m, MonadError QErr m) =>
      m a ->
      m a
  }

-- | Monads which support resource (memory, CPU time, etc.) limiting
class (Monad m) => HasResourceLimits m where
  askHTTPHandlerLimit :: m ResourceLimits
  askGraphqlOperationLimit :: HGE.RequestId -> UserInfo -> ApiLimit -> m ResourceLimits

  -- A default for monad transformer instances
  default askHTTPHandlerLimit ::
    (m ~ t n, MonadTrans t, HasResourceLimits n) =>
    m ResourceLimits
  askHTTPHandlerLimit = lift askHTTPHandlerLimit

  default askGraphqlOperationLimit ::
    (m ~ t n, MonadTrans t, HasResourceLimits n) =>
    HGE.RequestId ->
    UserInfo ->
    ApiLimit ->
    m ResourceLimits
  askGraphqlOperationLimit reqId userInfo apiLimit = lift $ askGraphqlOperationLimit reqId userInfo apiLimit

instance (HasResourceLimits m) => HasResourceLimits (ReaderT r m)

instance (HasResourceLimits m) => HasResourceLimits (ExceptT e m)

instance (HasResourceLimits m) => HasResourceLimits (Tracing.TraceT m)
