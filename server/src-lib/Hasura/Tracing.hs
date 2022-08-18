{-# LANGUAGE UndecidableInstances #-}

module Hasura.Tracing
  ( MonadTrace (..),
    TraceT,
    runTraceT,
    runTraceTWith,
    runTraceTWithReporter,
    runTraceTInContext,
    interpTraceT,
    TraceContext (..),
    Reporter (..),
    noReporter,
    HasReporter (..),
    TracingMetadata,
    extractHttpContext,
    tracedHttpRequest,
    injectEventContext,
    extractEventContext,
    word64ToHex,
  )
where

import Control.Lens (over, view, (^?))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.Aeson.Lens qualified as JL
import Data.Binary qualified as Bin
import Data.ByteString.Base16 qualified as Hex
import Data.ByteString.Lazy qualified as BL
import Data.String (fromString)
import Hasura.Prelude
import Network.HTTP.Client.Manager (HasHttpManagerM (..))
import Network.HTTP.Client.Transformable qualified as HTTP
import System.Random qualified as Rand
import Web.HttpApiData qualified as HTTP

-- | Any additional human-readable key-value pairs relevant
-- to the execution of a block of code.
type TracingMetadata = [(Text, Text)]

newtype Reporter = Reporter
  { runReporter ::
      forall io a.
      MonadIO io =>
      TraceContext ->
      -- the current trace context
      Text ->
      -- human-readable name for this block of code
      io (a, TracingMetadata) ->
      -- the action whose execution we want to report, returning
      -- any metadata emitted
      io a
  }

noReporter :: Reporter
noReporter = Reporter \_ _ -> fmap fst

-- | A type class for monads which support some way to report execution traces.
--
-- See @instance Tracing.HasReporter (AppM impl)@ in @HasuraPro.App@.
class Monad m => HasReporter m where
  -- | Get the current tracer
  askReporter :: m Reporter
  default askReporter :: m Reporter
  askReporter = pure noReporter

instance HasReporter m => HasReporter (ReaderT r m) where
  askReporter = lift askReporter

instance HasReporter m => HasReporter (ExceptT e m) where
  askReporter = lift askReporter

-- | A trace context records the current active trace,
-- the active span within that trace, and the span's parent,
-- unless the current span is the root.
data TraceContext = TraceContext
  { -- | TODO what is this exactly? The topmost span id?
    tcCurrentTrace :: !Word64,
    tcCurrentSpan :: !Word64,
    tcCurrentParent :: !(Maybe Word64)
  }

-- | The 'TraceT' monad transformer adds the ability to keep track of
-- the current trace context.
newtype TraceT m a = TraceT {unTraceT :: ReaderT (TraceContext, Reporter) (WriterT TracingMetadata m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadMask, MonadCatch, MonadThrow, MonadBase b, MonadBaseControl b)

instance MonadTrans TraceT where
  lift = TraceT . lift . lift

instance MFunctor TraceT where
  hoist f (TraceT rwma) = TraceT (hoist (hoist f) rwma)

instance MonadError e m => MonadError e (TraceT m) where
  throwError = lift . throwError
  catchError (TraceT m) f = TraceT (catchError m (unTraceT . f))

instance MonadReader r m => MonadReader r (TraceT m) where
  ask = TraceT $ lift ask
  local f m = TraceT $ mapReaderT (local f) (unTraceT m)

instance (HasHttpManagerM m) => HasHttpManagerM (TraceT m) where
  askHttpManager = lift askHttpManager

-- | Run an action in the 'TraceT' monad transformer.
-- 'runTraceT' delimits a new trace with its root span, and the arguments
-- specify a name and metadata for that span.
runTraceT :: (HasReporter m, MonadIO m) => Text -> TraceT m a -> m a
runTraceT name tma = do
  rep <- askReporter
  runTraceTWithReporter rep name tma

runTraceTWith :: MonadIO m => TraceContext -> Reporter -> Text -> TraceT m a -> m a
runTraceTWith ctx rep name tma =
  runReporter rep ctx name $
    runWriterT $
      runReaderT (unTraceT tma) (ctx, rep)

-- | Run an action in the 'TraceT' monad transformer in an
-- existing context.
runTraceTInContext :: (MonadIO m, HasReporter m) => TraceContext -> Text -> TraceT m a -> m a
runTraceTInContext ctx name tma = do
  rep <- askReporter
  runTraceTWith ctx rep name tma

-- | Run an action in the 'TraceT' monad transformer in an
-- existing context.
runTraceTWithReporter :: MonadIO m => Reporter -> Text -> TraceT m a -> m a
runTraceTWithReporter rep name tma = do
  ctx <-
    TraceContext
      <$> liftIO Rand.randomIO
      <*> liftIO Rand.randomIO
      <*> pure Nothing
  runTraceTWith ctx rep name tma

-- | Monads which support tracing. 'TraceT' is the standard example.
class Monad m => MonadTrace m where
  -- | Trace the execution of a block of code, attaching a human-readable name.
  trace :: Text -> m a -> m a

  -- | Ask for the current tracing context, so that we can provide it to any
  -- downstream services, e.g. in HTTP headers.
  currentContext :: m TraceContext

  -- | Ask for the current tracing reporter
  currentReporter :: m Reporter

  -- | Log some metadata to be attached to the current span
  attachMetadata :: TracingMetadata -> m ()

-- | Reinterpret a 'TraceT' action in another 'MonadTrace'.
-- This can be useful when you need to reorganize a monad transformer stack, for
-- example, to embed an action in some monadic computation, while preserving tracing
-- metadata and context.
--
-- For example, we use this function in various places in 'BackendExecute',
-- where we receive an action to execute in some concrete monad transformer stack.
-- See the various implementations of 'runQuery' for examples.
-- Ideally, the input computation's type would be sufficiently polymorphic that
-- we would not need to reorder monads inthe transformer stack. However, the monad
-- transformer stacks must be concrete, because their types are defined by
-- an associated type family 'ExecutionMonad'. Hence, we need to use this function
-- to peel off the outermost 'TraceT' constructor, and embed the computation in some
-- other 'MonadTrace'.
--
-- A second example is related to caching. The 'cacheLookup' function returns an
-- action in a concrete transformer stack, again because we are constrained by the
-- usage of a type class. We need to reinterpret the 'TraceT' component of this
-- concrete stack in some other abstract monad transformer stack, using this function.
--
-- Laws:
--
-- > interpTraceT id (hoist f (TraceT x)) = interpTraceT f (TraceT x)
interpTraceT ::
  MonadTrace n =>
  (m (a, TracingMetadata) -> n (b, TracingMetadata)) ->
  TraceT m a ->
  n b
interpTraceT f (TraceT rwma) = do
  ctx <- currentContext
  rep <- currentReporter
  (b, meta) <- f (runWriterT (runReaderT rwma (ctx, rep)))
  attachMetadata meta
  pure b

-- | If the underlying monad can report trace data, then 'TraceT' will
-- collect it and hand it off to that reporter.
instance MonadIO m => MonadTrace (TraceT m) where
  -- Note: this implementation is so awkward because we don't want to give the
  -- derived MonadReader/Writer instances to TraceT
  trace name ma = TraceT . ReaderT $ \(ctx, rep) -> do
    spanId <- liftIO (Rand.randomIO :: IO Word64)
    let subCtx =
          ctx
            { tcCurrentSpan = spanId,
              tcCurrentParent = Just (tcCurrentSpan ctx)
            }
    lift . runReporter rep subCtx name . runWriterT $ runReaderT (unTraceT ma) (subCtx, rep)

  currentContext = TraceT (asks fst)

  currentReporter = TraceT (asks snd)

  attachMetadata = TraceT . tell

instance MonadTrace m => MonadTrace (ReaderT r m) where
  trace = mapReaderT . trace
  currentContext = lift currentContext
  currentReporter = lift currentReporter
  attachMetadata = lift . attachMetadata

instance MonadTrace m => MonadTrace (StateT e m) where
  trace = mapStateT . trace
  currentContext = lift currentContext
  currentReporter = lift currentReporter
  attachMetadata = lift . attachMetadata

instance MonadTrace m => MonadTrace (ExceptT e m) where
  trace = mapExceptT . trace
  currentContext = lift currentContext
  currentReporter = lift currentReporter
  attachMetadata = lift . attachMetadata

-- | Encode Word64 to 16 character hex string
word64ToHex :: Word64 -> Text
word64ToHex randNum = bsToTxt $ Hex.encode numInBytes
  where
    numInBytes = BL.toStrict (Bin.encode randNum)

-- | Decode 16 character hex string to Word64
hexToWord64 :: Text -> Maybe Word64
hexToWord64 randText = do
  case Hex.decode $ txtToBs randText of
    Left _ -> Nothing
    Right decoded -> Just $ Bin.decode $ BL.fromStrict decoded

-- | Inject the trace context as a set of HTTP headers.
injectHttpContext :: TraceContext -> [HTTP.Header]
injectHttpContext TraceContext {..} =
  ("X-B3-TraceId", txtToBs $ word64ToHex tcCurrentTrace) :
  ("X-B3-SpanId", txtToBs $ word64ToHex tcCurrentSpan) :
    [ ("X-B3-ParentSpanId", txtToBs $ word64ToHex parentID)
      | parentID <- maybeToList tcCurrentParent
    ]

-- | Extract the trace and parent span headers from a HTTP request
-- and create a new 'TraceContext'. The new context will contain
-- a fresh span ID, and the provided span ID will be assigned as
-- the immediate parent span.
extractHttpContext :: [HTTP.Header] -> IO (Maybe TraceContext)
extractHttpContext hdrs = do
  freshSpanId <- liftIO Rand.randomIO
  pure $
    TraceContext
      <$> (hexToWord64 =<< HTTP.parseHeaderMaybe =<< lookup "X-B3-TraceId" hdrs)
      <*> pure freshSpanId
      <*> pure (hexToWord64 =<< HTTP.parseHeaderMaybe =<< lookup "X-B3-SpanId" hdrs)

-- | Inject the trace context as a JSON value, appropriate for
-- storing in (e.g.) an event trigger payload.
injectEventContext :: TraceContext -> J.Value
injectEventContext TraceContext {..} =
  J.object
    [ "trace_id" J..= word64ToHex tcCurrentTrace,
      "span_id" J..= word64ToHex tcCurrentSpan
    ]

-- | Extract a trace context from an event trigger payload.
extractEventContext :: J.Value -> IO (Maybe TraceContext)
extractEventContext e = do
  freshSpanId <- liftIO Rand.randomIO
  pure $
    TraceContext
      <$> (hexToWord64 =<< e ^? JL.key "trace_context" . JL.key "trace_id" . JL._String)
      <*> pure freshSpanId
      <*> pure (hexToWord64 =<< e ^? JL.key "trace_context" . JL.key "span_id" . JL._String)

-- | Perform HTTP request which supports Trace headers using a
-- HTTP.Request value
--
-- TODO REFACTOR:
--   - inline 'HTTP.performRequest' so that we can be sure a trace is always logged
--   - Inline 'try' here since we always use that at call sites
tracedHttpRequest ::
  MonadTrace m =>
  -- | http request that needs to be made
  HTTP.Request ->
  -- | a function that takes the traced request and executes it
  (HTTP.Request -> m a) ->
  m a
tracedHttpRequest req f = do
  let method = bsToTxt (view HTTP.method req)
      uri = view HTTP.url req
  trace (method <> " " <> uri) do
    let reqBytes = HTTP.getReqSize req
    attachMetadata [("request_body_bytes", fromString (show reqBytes))]
    ctx <- currentContext
    f $ over HTTP.headers (injectHttpContext ctx <>) req
