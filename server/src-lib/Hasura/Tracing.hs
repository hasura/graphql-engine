{-# LANGUAGE UndecidableInstances #-}

module Hasura.Tracing
  ( MonadTrace (..),
    TraceT,
    runTraceT,
    runTraceTWith,
    runTraceTWithReporter,
    runTraceTInContext,
    ignoreTraceT,
    interpTraceT,
    TraceContext (..),
    Reporter (..),
    noReporter,
    HasReporter (..),
    SamplingPolicy,
    sampleNever,
    sampleAlways,
    sampleRandomly,
    sampleOneInN,
    TracingMetadata,
    extractB3HttpContext,
    tracedHttpRequest,
    injectEventContext,
    extractEventContext,
  )
where

import Control.Lens (over, view, (^?))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Data.Aeson qualified as J
import Data.Aeson.Lens qualified as JL
import Data.ByteString.Char8 qualified as Char8
import Data.IORef
import Data.String (fromString)
import Hasura.Prelude
import Hasura.Tracing.TraceId
  ( SpanId,
    TraceId,
    randomSpanId,
    randomTraceId,
    spanIdFromHex,
    spanIdToHex,
    traceIdFromHex,
    traceIdToHex,
  )
import Network.HTTP.Client.Manager (HasHttpManagerM (..))
import Network.HTTP.Client.Transformable qualified as HTTP
import Refined (Positive, Refined, unrefine)
import System.Random.Stateful qualified as Random

-- | Any additional human-readable key-value pairs relevant
-- to the execution of a block of code.
type TracingMetadata = [(Text, Text)]

newtype Reporter = Reporter
  { runReporter ::
      forall io a.
      (MonadIO io, MonadBaseControl IO io) =>
      TraceContext ->
      -- the current trace context
      Text ->
      -- human-readable name for this block of code
      IO TracingMetadata ->
      -- an IO action that gets all of the metadata logged so far by the action
      -- being traced
      io a ->
      -- the action we want to trace
      io a
  }

noReporter :: Reporter
noReporter = Reporter \_ _ _ -> id

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

instance HasReporter IO

-- | A trace context records the current active trace,
-- the active span within that trace, and the span's parent,
-- unless the current span is the root.
data TraceContext = TraceContext
  { -- | TODO what is this exactly? The topmost span id?
    tcCurrentTrace :: !TraceId,
    tcCurrentSpan :: !SpanId,
    tcCurrentParent :: !(Maybe SpanId),
    tcSamplingState :: !SamplingState
  }

-- | B3 propagation sampling state.
--
-- Debug sampling state not represented.
data SamplingState = SamplingDefer | SamplingDeny | SamplingAccept

-- | Convert a sampling state to a value for the X-B3-Sampled header. A return
-- value of Nothing indicates that the header should not be set.
samplingStateToHeader :: IsString s => SamplingState -> Maybe s
samplingStateToHeader = \case
  SamplingDefer -> Nothing
  SamplingDeny -> Just "0"
  SamplingAccept -> Just "1"

-- | Convert a X-B3-Sampled header value to a sampling state. An input of
-- Nothing indicates that the header was not set.
samplingStateFromHeader :: (IsString s, Eq s) => Maybe s -> SamplingState
samplingStateFromHeader = \case
  Nothing -> SamplingDefer
  Just "0" -> SamplingDeny
  Just "1" -> SamplingAccept
  Just _ -> SamplingDefer

data TraceTEnv = TraceTEnv
  { tteTraceContext :: TraceContext,
    tteReporter :: Reporter,
    tteMetadataRef :: IORef TracingMetadata,
    tteSamplingDecision :: SamplingDecision
  }

-- | A local decision about whether or not to sample spans.
data SamplingDecision = SampleNever | SampleAlways

-- | An IO action for deciding whether or not to sample a trace.
--
-- Currently restricted to deny access to the B3 sampling state, but we may
-- want to be more flexible in the future.
type SamplingPolicy = IO SamplingDecision

-- Helper for consistently deciding whether or not to sample a trace based on
-- trace context and sampling policy.
decideSampling :: SamplingState -> SamplingPolicy -> IO SamplingDecision
decideSampling samplingState samplingPolicy =
  case samplingState of
    SamplingDefer -> samplingPolicy
    SamplingDeny -> pure SampleNever
    SamplingAccept -> pure SampleAlways

-- Helper for consistently updating the sampling state when a sampling decision
-- is made.
updateSamplingState :: SamplingDecision -> SamplingState -> SamplingState
updateSamplingState samplingDecision = \case
  SamplingDefer ->
    case samplingDecision of
      SampleNever -> SamplingDefer
      SampleAlways -> SamplingAccept
  SamplingDeny -> SamplingDeny
  SamplingAccept -> SamplingAccept

sampleNever :: SamplingPolicy
sampleNever = pure SampleNever

sampleAlways :: SamplingPolicy
sampleAlways = pure SampleAlways

-- @sampleRandomly p@ returns `SampleAlways` with probability @p@ and
-- `SampleNever` with probability @1 - p@.
sampleRandomly :: Double -> SamplingPolicy
sampleRandomly samplingProbability
  | samplingProbability <= 0 = pure SampleNever
  | samplingProbability >= 1 = pure SampleAlways
  | otherwise = do
      x <- Random.uniformRM (0, 1) Random.globalStdGen
      pure $ if x < samplingProbability then SampleAlways else SampleNever

-- Like @sampleRandomly@, but with the probability expressed as the denominator
-- N of the fraction 1/N.
sampleOneInN :: Refined Positive Int -> SamplingPolicy
sampleOneInN denominator
  | n == 1 = pure SampleAlways
  | otherwise = do
      x <- Random.uniformRM (0, n - 1) Random.globalStdGen
      pure $ if x == 0 then SampleAlways else SampleNever
  where
    n = unrefine denominator

-- | The 'TraceT' monad transformer adds the ability to keep track of
-- the current trace context.
newtype TraceT m a = TraceT {unTraceT :: ReaderT TraceTEnv m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadMask, MonadCatch, MonadThrow, MonadBase b, MonadBaseControl b)

instance MonadTrans TraceT where
  lift = TraceT . lift

instance MFunctor TraceT where
  hoist f (TraceT rwma) = TraceT (hoist f rwma)

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
runTraceT ::
  (HasReporter m, MonadIO m, MonadBaseControl IO m) =>
  SamplingPolicy ->
  Text ->
  TraceT m a ->
  m a
runTraceT policy name tma = do
  rep <- askReporter
  runTraceTWithReporter rep policy name tma

runTraceTWith ::
  (MonadIO m, MonadBaseControl IO m) =>
  TraceContext ->
  Reporter ->
  SamplingPolicy ->
  Text ->
  TraceT m a ->
  m a
runTraceTWith ctx rep policy name tma = do
  samplingDecision <- liftIO $ decideSampling (tcSamplingState ctx) policy
  metadataRef <- liftIO $ newIORef []
  let subCtx =
        ctx
          { tcSamplingState =
              updateSamplingState samplingDecision (tcSamplingState ctx)
          }
      report =
        case samplingDecision of
          SampleNever -> id
          SampleAlways -> do
            runReporter rep ctx name (readIORef metadataRef)
  report $
    runReaderT (unTraceT tma) (TraceTEnv subCtx rep metadataRef samplingDecision)

-- | Run an action in the 'TraceT' monad transformer in an
-- existing context.
runTraceTInContext ::
  (MonadIO m, MonadBaseControl IO m, HasReporter m) =>
  TraceContext ->
  SamplingPolicy ->
  Text ->
  TraceT m a ->
  m a
runTraceTInContext ctx policy name tma = do
  rep <- askReporter
  runTraceTWith ctx rep policy name tma

-- | Run an action in the 'TraceT' monad transformer in an
-- existing context.
runTraceTWithReporter ::
  (MonadIO m, MonadBaseControl IO m) =>
  Reporter ->
  SamplingPolicy ->
  Text ->
  TraceT m a ->
  m a
runTraceTWithReporter rep policy name tma = do
  ctx <-
    TraceContext
      <$> liftIO randomTraceId
      <*> liftIO randomSpanId
      <*> pure Nothing
      <*> pure SamplingDefer
  runTraceTWith ctx rep policy name tma

-- | Run an action in the 'TraceT' monad transformer while suppressing all
-- tracing-related side-effects.
ignoreTraceT :: (MonadIO m, MonadBaseControl IO m) => TraceT m a -> m a
ignoreTraceT = runTraceTWithReporter noReporter sampleNever ""

-- | Monads which support tracing. 'TraceT' is the standard example.
class Monad m => MonadTrace m where
  -- | Trace the execution of a block of code, attaching a human-readable name.
  trace :: Text -> m a -> m a

  -- | Ask for the current tracing context, so that we can provide it to any
  -- downstream services, e.g. in HTTP headers.
  currentContext :: m TraceContext

  -- | Ask for the current tracing reporter
  currentReporter :: m Reporter

  -- | Ask for the current handle on the tracing metadata
  currentMetadataRef :: m (IORef TracingMetadata)

  -- | Ask for the current sampling decision
  currentSamplingDecision :: m SamplingDecision

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
interpTraceT :: MonadTrace n => (m a -> n b) -> TraceT m a -> n b
interpTraceT f (TraceT rma) = do
  ctx <- currentContext
  rep <- currentReporter
  metadataRef <- currentMetadataRef
  samplingDecision <- currentSamplingDecision
  f (runReaderT rma (TraceTEnv ctx rep metadataRef samplingDecision))

-- | If the underlying monad can report trace data, then 'TraceT' will
-- collect it and hand it off to that reporter.
instance (MonadIO m, MonadBaseControl IO m) => MonadTrace (TraceT m) where
  -- Note: this implementation is so awkward because we don't want to give the
  -- derived MonadReader/Writer instances to TraceT
  trace name ma =
    TraceT $
      ReaderT $ \env@(TraceTEnv ctx rep _ samplingDecision) -> do
        case samplingDecision of
          SampleNever -> runReaderT (unTraceT ma) env
          SampleAlways -> do
            spanId <- liftIO randomSpanId
            let subCtx =
                  ctx
                    { tcCurrentSpan = spanId,
                      tcCurrentParent = Just (tcCurrentSpan ctx)
                    }
            metadataRef <- liftIO $ newIORef []
            runReporter rep subCtx name (readIORef metadataRef) $
              runReaderT
                (unTraceT ma)
                (TraceTEnv subCtx rep metadataRef samplingDecision)

  currentContext = TraceT (asks tteTraceContext)

  currentReporter = TraceT (asks tteReporter)

  currentMetadataRef = TraceT (asks tteMetadataRef)

  currentSamplingDecision = TraceT (asks tteSamplingDecision)

  attachMetadata metadata =
    TraceT $
      ReaderT $ \env ->
        liftIO $ modifyIORef' (tteMetadataRef env) (metadata ++)

instance MonadTrace m => MonadTrace (ReaderT r m) where
  trace = mapReaderT . trace
  currentContext = lift currentContext
  currentReporter = lift currentReporter
  currentMetadataRef = lift currentMetadataRef
  currentSamplingDecision = lift currentSamplingDecision
  attachMetadata = lift . attachMetadata

instance MonadTrace m => MonadTrace (StateT e m) where
  trace = mapStateT . trace
  currentContext = lift currentContext
  currentReporter = lift currentReporter
  currentMetadataRef = lift currentMetadataRef
  currentSamplingDecision = lift currentSamplingDecision
  attachMetadata = lift . attachMetadata

instance MonadTrace m => MonadTrace (ExceptT e m) where
  trace = mapExceptT . trace
  currentContext = lift currentContext
  currentReporter = lift currentReporter
  currentMetadataRef = lift currentMetadataRef
  currentSamplingDecision = lift currentSamplingDecision
  attachMetadata = lift . attachMetadata

-- | Inject the trace context as a set of HTTP headers.
injectB3HttpContext :: TraceContext -> [HTTP.Header]
injectB3HttpContext TraceContext {..} =
  let traceId = (b3HeaderTraceId, traceIdToHex tcCurrentTrace)
      spanId = (b3HeaderSpanId, spanIdToHex tcCurrentSpan)
      parentSpanIdMaybe =
        (,) b3HeaderParentSpanId . spanIdToHex <$> tcCurrentParent
      samplingStateMaybe =
        (,) b3HeaderSampled <$> samplingStateToHeader tcSamplingState
   in traceId : spanId : catMaybes [parentSpanIdMaybe, samplingStateMaybe]

-- | Extract the trace and parent span headers from a HTTP request
-- and create a new 'TraceContext'. The new context will contain
-- a fresh span ID, and the provided span ID will be assigned as
-- the immediate parent span.
extractB3HttpContext :: [HTTP.Header] -> IO (Maybe TraceContext)
extractB3HttpContext hdrs = do
  -- B3 TraceIds can have a length of either 64 bits (16 hex chars) or 128 bits
  -- (32 hex chars). For 64-bit TraceIds, we pad them with zeros on the left to
  -- make them 128 bits long.
  let traceIdMaybe =
        lookup b3HeaderTraceId hdrs >>= \rawTraceId ->
          if
              | Char8.length rawTraceId == 32 ->
                  traceIdFromHex rawTraceId
              | Char8.length rawTraceId == 16 ->
                  traceIdFromHex $ Char8.replicate 16 '0' <> rawTraceId
              | otherwise ->
                  Nothing
  for traceIdMaybe $ \traceId -> do
    freshSpanId <- liftIO randomSpanId
    let parentSpanId = spanIdFromHex =<< lookup b3HeaderSpanId hdrs
        samplingState = samplingStateFromHeader $ lookup b3HeaderSampled hdrs
    pure $ TraceContext traceId freshSpanId parentSpanId samplingState

b3HeaderTraceId, b3HeaderSpanId, b3HeaderParentSpanId, b3HeaderSampled :: IsString s => s
b3HeaderTraceId = "X-B3-TraceId"
b3HeaderSpanId = "X-B3-SpanId"
b3HeaderParentSpanId = "X-B3-ParentSpanId"
b3HeaderSampled = "X-B3-Sampled"

-- | Inject the trace context as a JSON value, appropriate for
-- storing in (e.g.) an event trigger payload.
injectEventContext :: TraceContext -> J.Value
injectEventContext TraceContext {..} =
  let idFields =
        [ eventKeyTraceId J..= bsToTxt (traceIdToHex tcCurrentTrace),
          eventKeySpanId J..= bsToTxt (spanIdToHex tcCurrentSpan)
        ]
      samplingFieldMaybe =
        (J..=) eventKeySamplingState <$> samplingStateToHeader @Text tcSamplingState
   in J.object $ idFields ++ maybeToList samplingFieldMaybe

-- | Extract a trace context from an event trigger payload.
extractEventContext :: J.Value -> IO (Maybe TraceContext)
extractEventContext e = do
  let traceIdMaybe =
        traceIdFromHex . txtToBs
          =<< e ^? JL.key "trace_context" . JL.key eventKeyTraceId . JL._String
  for traceIdMaybe $ \traceId -> do
    freshSpanId <- randomSpanId
    let parentSpanId =
          spanIdFromHex . txtToBs
            =<< e ^? JL.key "trace_context" . JL.key eventKeySpanId . JL._String
        samplingState =
          samplingStateFromHeader $
            e ^? JL.key "trace_context" . JL.key eventKeySamplingState . JL._String
    pure $ TraceContext traceId freshSpanId parentSpanId samplingState

eventKeyTraceId, eventKeySpanId, eventKeySamplingState :: J.Key
eventKeyTraceId = "trace_id"
eventKeySpanId = "span_id"
eventKeySamplingState = "sampling_state"

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
    f $ over HTTP.headers (injectB3HttpContext ctx <>) req
