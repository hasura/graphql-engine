{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use onLeft" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- This module defines the metrics store and all of its operations using
-- the state type defined in "System.Metrics.Prometheus.Internal.State". The
-- interface presented in this module is then restricted in
-- "System.Metrics.Prometheus" to produce the final interface.
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change in any way whatsoever
-- and without any warning between minor versions of this package.
--
-- = Implementation summary
--
-- * We wrap the internal `State` in an `IORef`, making it suitable as a
--   global store.
--
-- * We wrap operations on the `State` (as `Registration`s) and allow
--   them to be composed, then run such compositions atomically using
--   `atomicModifyIORef'`. This allows for atomic operations on the
--   `Store`.
--
-- * We bind the deregistration `Handle`s of
--   "System.Metrics.Prometheus.Internal.State" to specific `IORef`s in
--   `deregisterHandles`, preventing the confusion of handles from
--   different `Store`s.
--
-- * We validate metric names, metric help text, and label names before
--   they are registered. (Label values are currently unchecked.)
--
-- * We allow metrics to be registered as either "mutable" or
--   "immutable". "Immutable" metrics are intended to be permanent
--   fixtures that cannot be touched once registered, while "mutable"
--   can be removed or replaced at will.
module System.Metrics.Prometheus.Internal.Store
  ( -- * The metric store
    -- $metric-store
    Store,
    newStore,

    -- * Identifying metrics
    Identifier (..),
    Name,
    Labels,
    Help,

    -- * Registering metrics
    -- $registering
    Registration,
    RegistrationError (..),
    registerPermanently,
    registerRemovably,
    registerRemovablyCatch,
    registerCounter,
    registerGauge,
    registerHistogram,
    registerGroup,
    registerUncheckedDynamicGroup,

    -- ** Validation
    ValidationError (..),

    -- ** Convenience functions
    -- $convenience
    createCounter,
    createGauge,
    createHistogram,

    -- * Sampling metrics
    -- $sampling
    Sample,
    sampleAll,
    Value (..),
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import System.Metrics.Prometheus.Counter (Counter)
import qualified System.Metrics.Prometheus.Counter as Counter
import System.Metrics.Prometheus.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Gauge as Gauge
import System.Metrics.Prometheus.Histogram (Histogram, HistogramSample)
import qualified System.Metrics.Prometheus.Histogram as Histogram
import System.Metrics.Prometheus.Internal.State hiding
  ( deregister,
    register,
    registerGroup,
    registerUncheckedDynamicGroup,
    sampleAll,
  )
import qualified System.Metrics.Prometheus.Internal.State as Internal
import System.Metrics.Prometheus.Validation
  ( isValidHelpText,
    isValidName,
  )
import Prelude hiding (read)

------------------------------------------------------------------------

-- * The metric store

-- | A mutable metric store.
newtype Store = Store (IORef State)

-- | Create a new, empty metric store.
newStore :: IO Store
newStore = Store <$> newIORef initialState

------------------------------------------------------------------------

-- * Registering metrics

-- | An action that registers one or more metrics to a metric store.
newtype Registration
  = Registration
      ( Mutability ->
        State ->
        Either RegistrationError (State, [Handle] -> [Handle])
      )

instance Semigroup Registration where
  Registration f <> Registration g =
    Registration $ \mutability state0 -> do
      (state1, h1) <- f mutability state0
      (state2, h2) <- g mutability state1
      pure (state2, h2 . h1)

instance Monoid Registration where
  mempty = Registration $ \_mutability state -> Right (state, id)

-- | Errors that can occur during the registration of metrics
data RegistrationError
  = ValidationError ValidationError
  | MetricIdentifierAlreadyUsed Name Labels
  deriving (Show)

instance Exception RegistrationError

-- | Atomically apply a registration action to a metrics store,
-- registering the metrics as "permanent" metrics that cannot be removed
-- or replaced. In case of collisions of metric identifiers (name and
-- labels), throws an exception.
--
-- Throws a 'RegistrationError' exception if
--
-- * the registration attempts to register a metric with the name and
--   labels of an existing metric in the store; or if
--
-- * the registration contains invalid metric names, label names, or
--   help text.
registerPermanently ::
  -- | Metric store
  Store ->
  -- | Registration action
  Registration ->
  IO ()
registerPermanently = fmap void . registerThrow Permanent

-- | Atomically apply a registration action to a metrics store,
-- registering the metrics as "removable" metrics that can later be
-- removed or replaced. Returns an action to (atomically) deregister the
-- newly registered metrics. In case of collisions of metric identifiers
-- (name and labels), replaces existing metrics if they are removable,
-- and throws an exception if they are permanent.

-- Throws a 'RegistrationError' exception if
--

-- * the registration attempts to register a metric with the name and

--   labels of an existing metric in the store, _unless_ the existing
--   metric was registered as a removable metric via 'registerRemovably;
--   or if
--

-- * the registration contains invalid metric names, label names, or

--   help text.
registerRemovably ::
  -- | Metric store
  Store ->
  -- | Registration action
  Registration ->
  IO (IO ())
registerRemovably = registerThrow Removable

-- | Like 'registerRemovably', but returns 'RegistrationError's via
-- 'Either' rather than throwing them.
registerRemovablyCatch ::
  -- | Metric store
  Store ->
  -- | Registration action
  Registration ->
  IO
    ( Either
        RegistrationError
        (IO ())
    )
-- \^ Deregistration action

registerRemovablyCatch = registerCatch Removable

registerThrow ::
  -- | Whether the metrics should be registered as permament or removable
  Mutability ->
  -- | Metric store
  Store ->
  -- | Registration action
  Registration ->
  -- | Deregistration action
  IO (IO ())
registerThrow mutability store registration = do
  result <- registerCatch mutability store registration
  case result of
    Left validationError -> throwIO validationError
    Right deregisterAction -> pure deregisterAction

registerCatch ::
  -- | Whether the metrics should be registered as permament or removable
  Mutability ->
  -- | Metric store
  Store ->
  -- | Registration action
  Registration ->
  IO
    ( Either
        RegistrationError
        (IO ())
    )
-- \^ Deregistration action

registerCatch mutability (Store stateRef) (Registration f) = do
  atomicModifyIORef' stateRef $ \state0 ->
    case f mutability state0 of
      Left validationError ->
        -- Preserve initial state on error
        (state0, Left validationError)
      Right (state1, handles') ->
        let deregisterAction =
              deregisterHandles (handles' []) stateRef
         in (state1, Right deregisterAction)

-- | Deregister the metrics referred to by the given handles.
deregisterHandles ::
  [Internal.Handle] ->
  IORef Internal.State ->
  IO ()
deregisterHandles handles stateRef =
  atomicModifyIORef' stateRef $ \state ->
    (foldl' (flip Internal.deregisterByHandle) state handles, ())

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter ::
  -- | Counter identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Action to read the current metric value
  IO Double ->
  -- | Registration action
  Registration
registerCounter identifier help !sample =
  registerGeneric identifier help (CounterS sample)

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge ::
  -- | Gauge identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Action to read the current metric value
  IO Double ->
  -- | Registration action
  Registration
registerGauge identifier help !sample =
  registerGeneric identifier help (GaugeS sample)

-- | Register a histogram metric. The provided action to read the value
-- must be thread-safe. Also see 'createHistogram.
registerHistogram ::
  -- | Histogram identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Action to read the current metric value
  IO HistogramSample ->
  -- | Registration action
  Registration
registerHistogram identifier help !sample =
  registerGeneric identifier help (HistogramS sample)

registerGeneric ::
  -- | Metric identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Sampling action
  MetricSampler ->
  -- | Registration action
  Registration
registerGeneric identifier help sample =
  Registration $ \mutability state0 -> do
    first ValidationError $ validateIdentifier identifier
    first ValidationError $ validateHelpText help
    checkIdentifierCollision mutability identifier state0
    let (state1, handle) =
          Internal.register identifier help sample mutability state0
    pure (state1, (:) handle)

registerGroup ::
  -- | Metric names and getter functions
  M.Map Name (Help, M.Map Labels (a -> Value)) ->
  -- | Action to sample the metric group
  IO a ->
  -- | Registration action
  Registration
registerGroup !getters !cb =
  Registration $ \mutability state0 -> do
    validateGroupGetters state0 mutability getters
    let (state1, handles) =
          Internal.registerGroup getters cb mutability state0
    pure (state1, (++) handles)

registerUncheckedDynamicGroup ::
  -- | Metric names and getter functions
  M.Map Name (Help, a -> M.Map Labels Value) ->
  -- | Action to sample the metric group
  IO a ->
  -- | Registration action
  Registration
registerUncheckedDynamicGroup !getters !cb =
  Registration $ \_ state0 -> do
    validateUncheckedDynamicGroupGetters getters
    let (state1, maybeHandle) =
          Internal.registerUncheckedDynamicGroup getters cb state0
    pure (state1, (++) (maybeToList maybeHandle))

------------------------------------------------------------------------

-- ** Validation

validateIdentifier :: Identifier -> Either ValidationError ()
validateIdentifier identifier = do
  validateMetricName (idName identifier)
  for_ (HashMap.keys (idLabels identifier)) validateLabelName

validateGroupGetters ::
  State ->
  Mutability ->
  M.Map Name (Help, M.Map Labels (a -> Value)) ->
  Either RegistrationError ()
validateGroupGetters state mutability getters =
  for_ (M.toList getters) $ \(metricName, (help, labelSetMap)) -> do
    first ValidationError $ validateMetricName metricName
    first ValidationError $ validateHelpText help
    for_ (M.keys labelSetMap) $ \labelSet -> do
      traverse_
        (first ValidationError . validateLabelName)
        (HashMap.keys labelSet)
      let identifier = Identifier metricName labelSet
      checkIdentifierCollision mutability identifier state

validateUncheckedDynamicGroupGetters ::
  M.Map Name (Help, a) -> Either RegistrationError ()
validateUncheckedDynamicGroupGetters getters =
  for_ (M.toList getters) $ \(metricName, (help, _)) -> do
    first ValidationError $ validateMetricName metricName
    first ValidationError $ validateHelpText help

validateMetricName :: Text -> Either ValidationError ()
validateMetricName labelName =
  unless (isValidName labelName) $
    Left (InvalidMetricName labelName)

validateLabelName :: Text -> Either ValidationError ()
validateLabelName labelName =
  unless (isValidName labelName) $
    Left (InvalidLabelName labelName)

validateHelpText :: Text -> Either ValidationError ()
validateHelpText help =
  unless (isValidHelpText help) $
    Left (InvalidHelpText help)

checkIdentifierCollision ::
  Mutability -> Identifier -> State -> Either RegistrationError ()
checkIdentifierCollision mutability identifier state =
  case Internal.lookupMutability identifier state of
    Nothing -> Right ()
    Just Permanent ->
      Left $
        MetricIdentifierAlreadyUsed
          (idName identifier)
          (idLabels identifier)
    Just Removable ->
      case mutability of
        Removable ->
          Right ()
        Permanent ->
          -- Rationale: Non-deterministic execution could result
          -- instead in this permanent metric being registered
          -- first and the removable metric being registered
          -- afterwards, triggering a previous case in which an
          -- error is thrown. To be consistent, we should also
          -- throw the same error.
          --
          -- Permanent metrics should be the only metric
          -- registered at their identifiers for the lifetime of
          -- the metrics store.
          Left $
            MetricIdentifierAlreadyUsed
              (idName identifier)
              (idLabels identifier)

data ValidationError
  = InvalidMetricName Text
  | InvalidLabelName Text
  | InvalidHelpText Text

instance Exception ValidationError

instance Show ValidationError where
  show (InvalidMetricName invalidName) =
    "Invalid Prometheus metric name: " ++ T.unpack invalidName
  show (InvalidLabelName invalidName) =
    "Invalid Prometheus label name: " ++ T.unpack invalidName
  show (InvalidHelpText invalidHelpText) =
    "Invalid Prometheus help text: " ++ T.unpack invalidHelpText

------------------------------------------------------------------------

-- ** Convenience functions

-- $convenience
-- These functions combine the creation of a mutable reference (e.g. a
-- `System.Metrics.Prometheus.Counter.Counter`) with registering that
-- reference as a permanent metric.

-- | Create and permanently register a zero-initialized counter.
--
-- Can throw 'RegistrationError' exceptions.
createCounter ::
  -- | Counter identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Metric store
  Store ->
  IO Counter
createCounter identifier help store = do
  counter <- Counter.new
  registerPermanently store $
    registerCounter identifier help (Counter.read counter)
  return counter

-- | Create and permanently register a zero-initialized gauge.
--
-- Can throw 'RegistrationError' exceptions.
createGauge ::
  -- | Gauge identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Metric store
  Store ->
  IO Gauge
createGauge identifier help store = do
  gauge <- Gauge.new
  registerPermanently store $
    registerGauge identifier help (Gauge.read gauge)
  return gauge

-- | Create and permanently register an empty histogram. The buckets of
-- the histogram are fixed and defined by the given upper bounds.
--
-- Can throw 'RegistrationError' exceptions.
createHistogram ::
  -- | Upper bounds of buckets
  [Histogram.UpperBound] ->
  -- | Histogram identifier
  Identifier ->
  -- | Metric documentation
  Help ->
  -- | Metric store
  Store ->
  IO Histogram
createHistogram upperBounds identifier help store = do
  histogram <- Histogram.new upperBounds
  registerPermanently store $
    registerHistogram identifier help (Histogram.read histogram)
  return histogram

------------------------------------------------------------------------

-- * Sampling metrics

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store -> IO Sample
sampleAll (Store store) = readIORef store >>= Internal.sampleAll
