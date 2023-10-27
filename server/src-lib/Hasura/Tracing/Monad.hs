{-# LANGUAGE UndecidableInstances #-}

module Hasura.Tracing.Monad
  ( TraceT (..),
    runTraceT,
    ignoreTraceT,
  )
where

import Control.Lens
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Morph
import Control.Monad.Trans.Control
import Data.IORef
import Hasura.Prelude
import Hasura.RQL.Types.Session (UserInfoM (..))
import Hasura.Server.Types (MonadGetPolicies (..))
import Hasura.Tracing.Class
import Hasura.Tracing.Context
import Hasura.Tracing.Reporter
import Hasura.Tracing.Sampling

--------------------------------------------------------------------------------
-- TraceT

-- | TraceT is the standard implementation of 'MonadTrace'. Via a 'Reader', it
-- keeps track of the default policy and reporter to use thoughout the stack, as
-- well as the current trace.
newtype TraceT m a = TraceT (ReaderT (Reporter, Maybe TraceEnv) m a)
  deriving
    ( Functor,
      MonadFail, -- only due to https://gitlab.haskell.org/ghc/ghc/-/issues/15681
      Applicative,
      Monad,
      MonadIO,
      MonadFix,
      MonadMask,
      MonadCatch,
      MonadThrow,
      MonadState s,
      MonadError e,
      MonadBase b,
      MonadBaseControl b
    )

-- | Runs the 'TraceT' monad, by providing the default reporter. This does NOT
-- start a trace.
--
-- TODO: we could change this to always start a trace with a default name? This
-- would allow us to guarantee that there is always a current trace, but this
-- might not always be the correct behaviour: in practice, we would end up
-- generating one that spans the entire lifetime of the engine if 'runTraceT'
-- were to be used from 'main'.
runTraceT :: Reporter -> TraceT m a -> m a
runTraceT reporter (TraceT m) = runReaderT m (reporter, Nothing)

-- | Run the 'TraceT' monad, but without actually tracing anything: no report
-- will be emitted, even if calls to 'newTraceWith' force the trace to be
-- sampled.
ignoreTraceT :: TraceT m a -> m a
ignoreTraceT = runTraceT noReporter

instance MonadTrans TraceT where
  lift = TraceT . lift

-- | Hides the fact that TraceT is a reader to the rest of the stack.
instance (MonadReader r m) => MonadReader r (TraceT m) where
  ask = lift ask
  local f (TraceT m) = TraceT $ mapReaderT (local f) m

instance (MonadIO m, MonadBaseControl IO m) => MonadTrace (TraceT m) where
  newTraceWith context policy name (TraceT body) = TraceT do
    reporter <- asks fst
    samplingDecision <- decideSampling (tcSamplingState context) policy
    metadataRef <- liftIO $ newIORef []
    let report = case samplingDecision of
          SampleNever -> id
          SampleAlways -> runReporter reporter context name (readIORef metadataRef)
        updatedContext =
          context
            { tcSamplingState = updateSamplingState samplingDecision (tcSamplingState context)
            }
        traceEnv = TraceEnv updatedContext metadataRef samplingDecision
    report $ local (_2 .~ Just traceEnv) body

  newSpanWith spanId name (TraceT body) = TraceT do
    (reporter, traceEnv) <- ask
    case traceEnv of
      -- we are not currently in a trace: ignore this span
      Nothing -> body
      Just env -> case teSamplingDecision env of
        -- this trace is not sampled: ignore this span
        SampleNever -> body
        SampleAlways -> do
          metadataRef <- liftIO $ newIORef []
          let subContext =
                (teTraceContext env)
                  { tcCurrentSpan = spanId,
                    tcCurrentParent = Just (tcCurrentSpan $ teTraceContext env)
                  }
              subTraceEnv =
                env
                  { teTraceContext = subContext,
                    teMetadataRef = metadataRef
                  }
          runReporter reporter subContext name (readIORef metadataRef)
            $ local (_2 .~ Just subTraceEnv) body

  attachMetadata metadata = TraceT do
    asks (fmap teMetadataRef . snd) >>= \case
      Nothing -> pure ()
      Just ref -> liftIO $ modifyIORef' ref (metadata ++)

instance (MonadIO m, MonadBaseControl IO m) => MonadTraceContext (TraceT m) where
  currentContext = TraceT $ asks $ fmap teTraceContext . snd

instance (UserInfoM m) => UserInfoM (TraceT m) where
  askUserInfo = lift askUserInfo

instance (MonadGetPolicies m) => MonadGetPolicies (TraceT m) where
  runGetApiTimeLimit = lift runGetApiTimeLimit
  runGetPrometheusMetricsGranularity = lift runGetPrometheusMetricsGranularity
  runGetModelInfoLogStatus = lift $ runGetModelInfoLogStatus

--------------------------------------------------------------------------------
-- Internal

-- | Information about the current trace and span.
data TraceEnv = TraceEnv
  { teTraceContext :: TraceContext,
    teMetadataRef :: IORef TraceMetadata,
    teSamplingDecision :: SamplingDecision
  }

-- Helper for consistently deciding whether or not to sample a trace based on
-- trace context and sampling policy.
decideSampling :: (MonadIO m) => SamplingState -> SamplingPolicy -> m SamplingDecision
decideSampling samplingState samplingPolicy =
  case samplingState of
    SamplingDefer -> liftIO samplingPolicy
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
