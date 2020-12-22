{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Tracing
  ( MonadTrace(..)
  , TraceT
  , runTraceT
  , runTraceTWith
  , runTraceTWithReporter
  , runTraceTInContext
  , interpTraceT
  , TraceContext(..)
  , Reporter(..)
  , noReporter
  , HasReporter(..)
  , TracingMetadata
  , extractHttpContext
  , tracedHttpRequest
  , injectEventContext
  , extractEventContext
  ) where

import           Hasura.Prelude
import           Control.Lens                ((^?))
import           Control.Monad.Trans.Control
import           Control.Monad.Morph
import           Control.Monad.Unique
import           Data.String                 (fromString)
import           Network.URI                 (URI)

import qualified Data.Aeson                  as J
import qualified Data.Aeson.Lens             as JL
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Network.HTTP.Client         as HTTP
import qualified Network.HTTP.Types.Header   as HTTP
import qualified System.Random               as Rand
import qualified Web.HttpApiData             as HTTP

import qualified Data.Binary                 as Bin
import qualified Data.ByteString.Base16      as Hex


-- | Any additional human-readable key-value pairs relevant
-- to the execution of a block of code.
type TracingMetadata = [(Text, Text)]

newtype Reporter = Reporter
  { runReporter
      :: forall io a
       . MonadIO io
      => TraceContext
      -- ^ the current trace context
      -> Text
      -- ^ human-readable name for this block of code
      -> io (a, TracingMetadata)
      -- ^ the action whose execution we want to report, returning
      -- any metadata emitted
      -> io a
  }

noReporter :: Reporter
noReporter = Reporter \_ _ -> fmap fst

-- | A type class for monads which support some
-- way to report execution traces.
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
  { tcCurrentTrace   :: !Word64
  , tcCurrentSpan    :: !Word64
  , tcCurrentParent  :: !(Maybe Word64)
  }

-- | The 'TraceT' monad transformer adds the ability to keep track of
-- the current trace context.
newtype TraceT m a = TraceT { unTraceT :: ReaderT (TraceContext, Reporter) (WriterT TracingMetadata m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnique)

instance MonadTrans TraceT where
  lift = TraceT . lift . lift

instance MFunctor TraceT where
  hoist f (TraceT rwma) = TraceT (hoist (hoist f) rwma)

deriving instance MonadBase b m => MonadBase b (TraceT m)
deriving instance MonadBaseControl b m => MonadBaseControl b (TraceT m)

instance MonadError e m => MonadError e (TraceT m) where
  throwError = lift . throwError
  catchError (TraceT m) f = TraceT (catchError m (unTraceT . f))

instance MonadReader r m => MonadReader r (TraceT m) where
  ask = TraceT $ lift ask
  local f m = TraceT $ mapReaderT (local f) (unTraceT m)

-- | Run an action in the 'TraceT' monad transformer.
-- 'runTraceT' delimits a new trace with its root span, and the arguments
-- specify a name and metadata for that span.
runTraceT :: (HasReporter m, MonadIO m) => Text -> TraceT m a -> m a
runTraceT name tma = do
  rep <- askReporter
  runTraceTWithReporter rep name tma

runTraceTWith :: MonadIO m => TraceContext -> Reporter -> Text -> TraceT m a -> m a
runTraceTWith ctx rep name tma =
  runReporter rep ctx name
    $ runWriterT
    $ runReaderT (unTraceT tma) (ctx, rep)

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
  ctx <- TraceContext
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
-- This can be useful when you need to reorganize a monad transformer stack.
interpTraceT
  :: MonadTrace n
  => (m (a, TracingMetadata) -> n (b, TracingMetadata))
  -> TraceT m a
  -> n b
interpTraceT f (TraceT rwma) = do
  ctx <- currentContext
  rep <- currentReporter
  (b, meta) <- f (runWriterT (runReaderT rwma (ctx, rep)))
  attachMetadata meta
  pure b

-- | If the underlying monad can report trace data, then 'TraceT' will
-- collect it and hand it off to that reporter.
instance MonadIO m => MonadTrace (TraceT m) where
  trace name ma = TraceT . ReaderT $ \(ctx, rep) -> do
    spanId <- liftIO (Rand.randomIO :: IO Word64)
    let subCtx = ctx { tcCurrentSpan = spanId
                     , tcCurrentParent = Just (tcCurrentSpan ctx)
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
  where numInBytes = BL.toStrict (Bin.encode  randNum)

-- | Decode 16 character hex string to Word64
hexToWord64 :: Text -> Maybe Word64
hexToWord64 randText = do
  case Hex.decode $ txtToBs randText of
    Left _ -> Nothing
    Right decoded -> Just $ Bin.decode $ BL.fromStrict decoded

-- | Inject the trace context as a set of HTTP headers.
injectHttpContext :: TraceContext -> [HTTP.Header]
injectHttpContext TraceContext{..} =
  ("X-B3-TraceId", txtToBs $ word64ToHex tcCurrentTrace)
  : ("X-B3-SpanId", txtToBs $ word64ToHex tcCurrentSpan)
  : [ ("X-B3-ParentSpanId", txtToBs $ word64ToHex parentID)
    | parentID <- maybeToList tcCurrentParent
    ]

-- | Extract the trace and parent span headers from a HTTP request
-- and create a new 'TraceContext'. The new context will contain
-- a fresh span ID, and the provided span ID will be assigned as
-- the immediate parent span.
extractHttpContext :: [HTTP.Header] -> IO (Maybe TraceContext)
extractHttpContext hdrs = do
  freshSpanId <- liftIO Rand.randomIO
  pure $ TraceContext
    <$> (hexToWord64 =<< HTTP.parseHeaderMaybe =<< lookup "X-B3-TraceId" hdrs)
    <*> pure freshSpanId
    <*> pure (hexToWord64 =<< HTTP.parseHeaderMaybe =<< lookup "X-B3-SpanId" hdrs)


-- | Inject the trace context as a JSON value, appropriate for
-- storing in (e.g.) an event trigger payload.
injectEventContext :: TraceContext -> J.Value
injectEventContext TraceContext{..} =
  J.object
    [ "trace_id" J..= tcCurrentTrace
    , "span_id"  J..= tcCurrentSpan
    ]

-- | Extract a trace context from an event trigger payload.
extractEventContext :: J.Value -> IO (Maybe TraceContext)
extractEventContext e = do
  freshSpanId <- liftIO Rand.randomIO
  pure $ TraceContext
    <$> (e ^? JL.key "trace_context" . JL.key "trace_id" . JL._Integral)
    <*> pure freshSpanId
    <*> pure (e ^? JL.key "trace_context" . JL.key "span_id" . JL._Integral)

-- | Perform HTTP request which supports Trace headers
tracedHttpRequest
  :: MonadTrace m
  => HTTP.Request
  -- ^ http request that needs to be made
  -> (HTTP.Request -> m a)
  -- ^ a function that takes the traced request and executes it
  -> m a
tracedHttpRequest req f = do
  let method = bsToTxt (HTTP.method req)
      uri = show @URI (HTTP.getUri req)
  trace (method <> " " <> fromString uri) do
    let reqBytes = case HTTP.requestBody req of
          HTTP.RequestBodyBS bs -> Just (fromIntegral (BS.length bs))
          HTTP.RequestBodyLBS bs -> Just (BL.length bs)
          HTTP.RequestBodyBuilder len _ -> Just len
          HTTP.RequestBodyStream len _ -> Just len
          _ -> Nothing
    for_ reqBytes \b ->
      attachMetadata [("request_body_bytes", fromString (show b))]
    ctx <- currentContext
    let req' = req { HTTP.requestHeaders =
                       injectHttpContext ctx <> HTTP.requestHeaders req
                   }
    f req'
