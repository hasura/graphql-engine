-- | Defines the Tracing API.
--
-- The 'MonadTrace' class defines the "public API" of this component.
module Hasura.Tracing.Class
  ( MonadTrace (..),
    MonadTraceContext (..),
    runNoMonadTraceContext,
    newTrace,
    newSpan,
  )
where

import Control.Monad.Morph
import Control.Monad.Trans.Maybe
import Hasura.Prelude
import Hasura.Tracing.Context
import Hasura.Tracing.Sampling
import Hasura.Tracing.TraceId
import Hasura.Tracing.TraceState qualified as TS

--------------------------------------------------------------------------------
-- MonadTrace

class (MonadTraceContext m) => MonadTrace m where
  -- | Trace the execution of a block of code, attaching a human-readable
  -- name. This starts a new trace and its corresponding root span, to which
  -- subsequent spans will be attached.
  newTraceWith ::
    TraceContext ->
    SamplingPolicy ->
    Text ->
    m a ->
    m a

  -- | Starts a new span within the current trace. No-op if there's no current
  -- trace.
  --
  -- TODO: we could rewrite this to start a new trace if there isn't one, using
  -- the default reporter and policy? This would guarantee that no span is ever
  -- lost, but would also risk reporting undesired spans.
  newSpanWith ::
    SpanId ->
    Text ->
    m a ->
    m a

  -- | Log some arbitrary metadata to be attached to the current span, if any.
  attachMetadata :: TraceMetadata -> m ()

instance (MonadTrace m) => MonadTrace (ReaderT r m) where
  newTraceWith c p n = mapReaderT (newTraceWith c p n)
  newSpanWith i n = mapReaderT (newSpanWith i n)
  attachMetadata = lift . attachMetadata

instance (MonadTrace m) => MonadTrace (StateT e m) where
  newTraceWith c p n = mapStateT (newTraceWith c p n)
  newSpanWith i n = mapStateT (newSpanWith i n)
  attachMetadata = lift . attachMetadata

instance (MonadTrace m) => MonadTrace (ExceptT e m) where
  newTraceWith c p n = mapExceptT (newTraceWith c p n)
  newSpanWith i n = mapExceptT (newSpanWith i n)
  attachMetadata = lift . attachMetadata

instance (MonadTrace m) => MonadTrace (MaybeT m) where
  newTraceWith c p n = mapMaybeT (newTraceWith c p n)
  newSpanWith i n = mapMaybeT (newSpanWith i n)
  attachMetadata = lift . attachMetadata

-- | Access to the current tracing context, factored out of 'MonadTrace' so we
-- can use it separately and dispatch the constraint in cases outside of a
-- MonadTrace context.
class (Monad m) => MonadTraceContext m where
  -- | Ask for the current tracing context, so that we can provide it to any
  -- downstream services, e.g. in HTTP headers. Returns 'Nothing' if we're not
  -- currently tracing anything.
  currentContext :: m (Maybe TraceContext)

instance (MonadTraceContext m) => MonadTraceContext (ReaderT r m) where
  currentContext = lift currentContext

instance (MonadTraceContext m) => MonadTraceContext (StateT e m) where
  currentContext = lift currentContext

instance (MonadTraceContext m) => MonadTraceContext (ExceptT e m) where
  currentContext = lift currentContext

instance (MonadTraceContext m) => MonadTraceContext (MaybeT m) where
  currentContext = lift currentContext

newtype NoMonadTraceContext f a = NoMonadTraceContext (IdentityT f a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Monad m) => MonadTraceContext (NoMonadTraceContext m) where
  currentContext = NoMonadTraceContext $ pure Nothing

-- | Provide an empty 'TraceContext' where we aren't in a 'MonadTrace' context.
runNoMonadTraceContext :: NoMonadTraceContext f a -> f a
runNoMonadTraceContext (NoMonadTraceContext (IdentityT f)) = f

--------------------------------------------------------------------------------
-- Trace helpers

-- | Create a new trace using a randomly-generated context.
newTrace :: (MonadIO m, MonadTrace m) => SamplingPolicy -> Text -> m a -> m a
newTrace policy name body = do
  traceId <- randomTraceId
  spanId <- randomSpanId
  let context = TraceContext traceId spanId Nothing SamplingDefer TS.emptyTraceState
  newTraceWith context policy name body

-- | Create a new span with a randomly-generated id.
newSpan :: (MonadIO m, MonadTrace m) => Text -> m a -> m a
newSpan name body = do
  spanId <- randomSpanId
  newSpanWith spanId name body
