{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Avoid restricted function" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Implementation note:
-- This module merely wraps and restricts the interface of
-- "System.Metrics.Prometheus.Internal.Store". That is, the functions presented in
-- this interface are exactly the same as their counterparts in
-- "System.Metrics.Prometheus", except that they have been restricted to work on
-- only a narrow, user-defined set of inputs.

-- |
-- This module is for defining metrics that can be monitored.
--
-- This is an API reference. For an introduction to this module and
-- examples of its use, see the tutorial.
module System.Metrics.Prometheus
  ( -- * Overview
    -- $overview

    -- * Naming metrics
    -- $naming

    -- * The metric store
    -- $metric-store
    Store,
    newStore,

    -- * Static metric annotations
    MetricType (..),

    -- ** Labels
    ToLabels (..),

    -- * Changing scope
    -- $scopes
    subset,

    -- ** Common scopes
    EmptyMetrics,
    emptyOf,
    AllMetrics (..),
    ofAll,

    -- * Registering and deregistering metrics
    -- $registering-and-deregistering

    -- ** Registering
    -- $registering
    registerPermanently,
    registerRemovably,
    registerRemovablyCatch,
    Registration,
    Internal.RegistrationError (..),
    Internal.ValidationError (..),
    registerCounter,
    registerGauge,
    registerHistogram,
    registerGroup,
    SamplingGroup (..),
    registerUncheckedDynamicGroup,
    UncheckedDynamicSamplingGroup (..),
    MetricValue,

    -- ** Convenience functions
    -- $convenience
    createCounter,
    createGauge,
    createHistogram,
    createCounterVector,
    createGaugeVector,
    createHistogramVector,

    -- * Sampling metrics
    -- $sampling
    sampleAll,
    Internal.Sample,
    Internal.Name,
    Internal.Help,
    Internal.Labels,
    Internal.Value (..),

    -- * Predefined metrics
    -- $predefined
    registerGcMetrics,
    GcMetrics (..),
  )
where

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import qualified GHC.Stats as Stats
import GHC.TypeLits
import qualified System.Metrics.Prometheus.Counter as Counter
import System.Metrics.Prometheus.CounterVector (CounterVector)
import qualified System.Metrics.Prometheus.CounterVector as CounterVector
import qualified System.Metrics.Prometheus.Gauge as Gauge
import System.Metrics.Prometheus.GaugeVector (GaugeVector)
import qualified System.Metrics.Prometheus.GaugeVector as GaugeVector
import System.Metrics.Prometheus.Histogram (HistogramSample)
import qualified System.Metrics.Prometheus.Histogram as Histogram
import System.Metrics.Prometheus.HistogramVector (HistogramVector)
import qualified System.Metrics.Prometheus.HistogramVector as HistogramVector
import qualified System.Metrics.Prometheus.Internal.Sample as Sample
import System.Metrics.Prometheus.Internal.Store (RegistrationError)
import qualified System.Metrics.Prometheus.Internal.Store as Internal

-- $overview
-- Metrics are used to monitor program behavior and performance. All
-- metrics have
--
--  * a name,
--
--  * a set of labels, and
--
--  * a way to get the metric's current value.
--
-- This module provides a way to register metrics in a global \"metric
-- store\". The store can then be used to get a snapshot of all
-- metrics. The store also serves as a central place to keep track of
-- all the program's metrics, both user and library defined.
--
-- This module also provides a way to register a number of predefined
-- metrics that are useful in most applications. See e.g.
-- 'registerGcMetrics'.

-- $naming
-- Compound metric names should be separated using underscores.
-- Example: @request_count@. Periods in the name imply namespacing.
-- Example: @\"myapp.users\"@. Some consumers of metrics will use
-- these namespaces to group metrics in e.g. UIs.
--
-- Libraries and frameworks that want to register their own metrics
-- should prefix them with a namespace, to avoid collision with
-- user-defined metrics and metrics defined by other libraries. For
-- example, the Snap web framework could prefix all its metrics with
-- @\"snap.\"@.
--
-- It's customary to suffix the metric name with a short string
-- explaining the metric's type e.g. using @\"_ms\"@ to denote
-- milliseconds.

------------------------------------------------------------------------

-- * The metric store

-- $metric-store
-- The metric store is a shared store of metrics. It allows several
-- disjoint components (e.g. libraries) to contribute to the set of
-- metrics exposed by an application. Libraries that want to provide a
-- set of metrics should define a register method, in the style of
-- 'registerGcMetrics', that registers the metrics in the 'Store'. The
-- register function should document which metrics are registered and
-- their types (i.e. counter, gauge, or histogram).
--
-- References to metric stores are parameterized a type that restricts
-- the kinds of metrics that may be added to or removed from the `Store`
-- through the reference. In other words, they are /scoped/ by their
-- type parameter. Users are expected to parameterize their metric
-- stores with custom GADTs that describe the kinds of metrics that may
-- be collected in their applications.

-- | A mutable metric store, parameterized by a type @metrics@ whose
-- values @v@ represent the classes of metrics that may be registered to
-- the store.
--
-- The metrics of each class @v :: metrics name metricType label@ have
-- their name, metric type, and possible label statically determined by
-- the respective type indices of @metrics@.
newtype Store (metrics :: Symbol -> Symbol -> MetricType -> Type -> Type)
  = Store Internal.Store

-- | Create a new, empty metric store.
newStore :: IO (Store metrics)
newStore = Store <$> Internal.newStore

------------------------------------------------------------------------

-- * Static metric annotations

-- | An enumeration of the types of metrics. To be used as types/kinds
-- via -XDataKinds.
data MetricType
  = CounterType
  | GaugeType
  | HistogramType

-- | The type of values sampled by each metric.
type family MetricValue (t :: MetricType) :: Type where
  MetricValue 'CounterType = Double
  MetricValue 'GaugeType = Double
  MetricValue 'HistogramType = HistogramSample

-- | The `Metrics.Value` constructor for each metric.
class ToMetricValue (t :: MetricType) where
  toMetricValue :: Proxy t -> MetricValue t -> Internal.Value

instance ToMetricValue 'CounterType where toMetricValue _ = Internal.Counter

instance ToMetricValue 'GaugeType where toMetricValue _ = Internal.Gauge

instance ToMetricValue 'HistogramType where toMetricValue _ = Internal.Histogram

-- | The default implementation of each metric.
type family MetricsImpl (t :: MetricType) where
  MetricsImpl 'CounterType = Counter.Counter
  MetricsImpl 'GaugeType = Gauge.Gauge
  MetricsImpl 'HistogramType = Histogram.Histogram

------------------------------------------------------------------------

-- ** Labels

-- | A class of types that can be converted to sets of key-value pairs
-- ("labels"), which are used to annotate metrics with metadata.
--
-- Each metric must be associated with a type from this typeclass. The
-- type determines the structure of the label sets that may be attached
-- to the metric.
--
-- For convenience, one may derive, via "GHC.Generics", a `ToLabels`
-- instance for any record that exclusively has fields of type `T.Text`.
--
-- For example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import qualified Data.Text as T
-- > import GHC.Generics
-- >
-- > import System.Metrics.Prometheus
-- >
-- > data MyLabels = MyLabels
-- >   { key1 :: T.Text
-- >   , key2 :: T.Text
-- >   } deriving (Generic)
-- >
-- > instance ToLabels MyLabels
--
-- >>> toLabels $ MyLabels { key1 = "value1", key2 = "value2" }
-- fromList [("key1","value1"),("key2","value2")]
class ToLabels a where
  toLabels :: a -> HashMap.HashMap T.Text T.Text
  default toLabels ::
    (Generic a, GToLabels (Rep a)) => a -> HashMap.HashMap T.Text T.Text
  toLabels x = gToLabels undefined (from x)
  {-# INLINE toLabels #-}

-- | Disallow labels altogether.
--
-- > toLabels () = HashMap.empty
instance ToLabels () where
  toLabels () = HashMap.empty
  {-# INLINE toLabels #-}

-- | Place no constraints on labels.
--
-- > toLabels @(HashMap Text Text) = id
instance ToLabels (HashMap.HashMap T.Text T.Text) where
  toLabels = id
  {-# INLINE toLabels #-}

------------------------------------------------------------------------

-- ** Deriving `ToLabels`

--

-- | Deriving instances of `ToLabels` for records that exclusively have
-- fields of type `Text`.
class GToLabels (f :: Type -> Type) where
  gToLabels :: T.Text -> f x -> HashMap.HashMap T.Text T.Text

-- Data (passthrough)
instance (GToLabels f) => GToLabels (D1 c f) where
  gToLabels name (M1 x) = gToLabels name x
  {-# INLINE gToLabels #-}

-- Constructor (passthrough)
instance (GToLabels f) => GToLabels (C1 c f) where
  gToLabels name (M1 x) = gToLabels name x
  {-# INLINE gToLabels #-}

-- Products (union)
instance (GToLabels f, GToLabels g) => GToLabels (f :*: g) where
  gToLabels name (x :*: y) =
    gToLabels name x `HashMap.union` gToLabels name y
  {-# INLINE gToLabels #-}

-- Record selectors (take record selector name)
instance
  (GToLabels f, KnownSymbol name) =>
  GToLabels (S1 ('MetaSel ('Just name) su ss ds) f)
  where
  gToLabels _name (M1 x) =
    let name' = T.pack $ symbolVal $ Proxy @name
     in gToLabels name' x
  {-# INLINE gToLabels #-}

-- Individual fields (take value, combine with name)
instance GToLabels (K1 i T.Text) where
  gToLabels name (K1 x) = HashMap.singleton name x
  {-# INLINE gToLabels #-}

------------------------------------------------------------------------

-- * Changing scope

-- $scopes
-- References to metric stores are parameterized by types that restrict
-- the kinds of metrics that may be added to or removed from the store
-- through the references. In other words, they are /scoped/ by their
-- type parameter.
--
-- It can be useful to create a new reference to a metric store that is
-- scoped to a subset of its metrics. This can be done as long as the
-- subset can be represented by a function (see `subset`).

-- | Create a new reference to a metric store with restricted scope.
--
-- For example:
--
-- > data AppMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
-- >   GcSubset ::
-- >     GcMetrics name help metricType labels ->
-- >     AppMetrics name help metricType labels
-- >
-- > subset' :: Store AppMetrics -> Store GcMetrics
-- > subset' = subset GcSubset
subset ::
  -- | Subset
  ( forall name help metricType labels.
    metricsSubset name help metricType labels ->
    metrics name help metricType labels
  ) ->
  -- | Reference
  Store metrics ->
  -- | Restricted reference
  Store metricsSubset
subset _ = coerce

-- `coerce` is a safe implementation for this function for the following
-- reasons.
--
-- The only effect of the metrics specification type parameter `metric`
-- of a store reference `Store metric` is to restrict the operations
-- that can be performed through the store reference. In particular, the
-- type parameter makes no guarantees about the contents of the store,
-- or the operations that have been performed on it. In other words,
-- there are no store invariants that we are responsible for preserving.
--
-- We only need to uphold the contract for this particular function,
-- which is that the set operations allowed by the `metricsSubset`
-- specification is a subset of that of the `metrics` specification. In
-- general, the set of allowed operations of a metrics specification
-- `metric` is determined by the set of triplets of types `(name,
-- metricType, labels)` for which there is a value `v :: metric name
-- metricType labels` in `metric`. Therefore, we only need to show that
-- for every value `w0 :: metricSubset name metricType value` in
-- `metricSubset`, there is a value `w1 :: metric name metricType value`
-- in `metric` with the same type parameters as `w0`. But this is
-- exactly what is done by a function of type `forall name metricType
-- labels. metricsSubset name metricType labels -> metrics name
-- metricType labels`.
--
-- Note: This function can be misused by providing an `undefined` value
-- for the subset function. Doing so yields a function that can set the
-- type parameter of a `Store` to an arbitrary type (of the proper
-- kind).

-- ** Common scopes

-- | The smallest scope, containing no metrics. This scope can be
-- embedded in any scope via the `emptyOf` function.
--
-- The only operation available to a store with scope @`EmptyMetrics`@
-- is `sampleAll`.
data EmptyMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type

-- | The smallest scope can be embedded in any scope.
emptyOf ::
  EmptyMetrics name help metricType labels ->
  metrics name help metricType labels
emptyOf metrics = case metrics of {}

-- | The largest scope, containing all metrics. All scopes can be
-- embedded in this scope via the `ofAll` function.
--
-- Metrics of any form may be registered to a store of type `AllMetrics`
-- using the `Metric` constructor. For example:
--
-- > example :: Store AllMetrics -> IO Counter.Counter
-- > example = createCounter (Metrics @"total_requests" @"") ()
data AllMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
  Metric :: AllMetrics name help metricType labels

_exampleAllMetrics :: Store AllMetrics -> IO Counter.Counter
_exampleAllMetrics = createCounter (Metric @"total_requests" @"") ()

-- | All scopes can be embedded in the largest scope.
ofAll ::
  metrics name help metricType labels ->
  AllMetrics name help metricType labels
ofAll _ = Metric

------------------------------------------------------------------------

-- * Registering and deregistering metrics

-- $registering-and-deregistering
-- Before metrics can be sampled, they need to be registered with the
-- metric store.

------------------------------------------------------------------------

-- ** Registering

-- $registering
-- Metrics are registered as either "permanent" or "removable".
-- Removable metrics can later be deregistered and replaced, while
-- permanent metrics cannot.
--
-- Metrics are identified by the combination of their metric name and
-- label set. If you try to register a metric at an identifier that is
-- already in use by an existing metric, what happens depends on whether
-- the metrics are permanent or removable. If both metrics are
-- removable, then the existing metric will be deregistered and replaced
-- by the new metric; in all other cases, an exception will be thrown.
--
-- Upon registering a set of removable metrics, you will be given a
-- handle that can be used to deregister the newly registered metrics
-- /specifically/, in the following sense. If a deregistration handle
-- targets a metric, and that metric is replaced by a new metric, the
-- new metric will not be deregistered if the handle is used.

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
  Store metrics ->
  -- | Registration action
  Registration metrics ->
  -- | Deregistration handle
  IO ()
registerPermanently (Store store) (Registration registration) =
  Internal.registerPermanently store registration

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
--   metric was registered as a removable metric via
--   'registerRemovably'; or if
--

-- * the registration contains invalid metric names, label names, or

--   help text.
registerRemovably ::
  -- | Metric store
  Store metrics ->
  -- | Registration action
  Registration metrics ->
  -- | Deregistration handle
  IO (IO ())
registerRemovably (Store store) (Registration registration) =
  Internal.registerRemovably store registration

-- | Like 'registerRemovably', but returns 'RegistrationError's via
-- 'Either' rather than throwing them.
registerRemovablyCatch ::
  -- | Metric store
  Store metrics ->
  -- | Registration action
  Registration metrics ->
  -- | Deregistration handle
  IO (Either RegistrationError (IO ()))
registerRemovablyCatch (Store store) (Registration registration) =
  Internal.registerRemovablyCatch store registration

-- | An action that registers one or more metrics to a metric store.
-- Can only be run by `register`.
newtype Registration (metric :: Symbol -> Symbol -> MetricType -> Type -> Type)
  = Registration Internal.Registration

-- | Combine registration actions by running one after the other.
deriving instance Semigroup (Registration metrics)

deriving instance Monoid (Registration metrics)

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  -- | Metric class
  metrics name help 'CounterType labels ->
  -- | Labels
  labels ->
  -- | Action to read the current metric value
  IO Double ->
  Registration metrics
registerCounter = registerGeneric Internal.registerCounter

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  -- | Metric class
  metrics name help 'GaugeType labels ->
  -- | Labels
  labels ->
  -- | Action to read the current metric value
  IO Double ->
  Registration metrics
registerGauge = registerGeneric Internal.registerGauge

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerHistogram ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  -- | Metric class
  metrics name help 'HistogramType labels ->
  -- | Labels
  labels ->
  -- | Action to read the current metric value
  IO HistogramSample ->
  Registration metrics
registerHistogram = registerGeneric Internal.registerHistogram

registerGeneric ::
  forall metrics name help metricType labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  ( Internal.Identifier ->
    Internal.Help ->
    IO (MetricValue metricType) ->
    Internal.Registration
  ) ->
  -- | Metric class
  metrics name help metricType labels ->
  -- | Labels
  labels ->
  -- | Action to read the current metric value
  IO (MetricValue metricType) ->
  -- | Registration action
  Registration metrics
registerGeneric f _ labels sample =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Internal.Identifier name (toLabels labels)
      help = T.pack $ symbolVal (Proxy @help)
   in Registration $ f identifier help sample

-- | Register an action that will be executed any time one of the
-- metrics computed from the value it returns needs to be sampled.
--
-- When one or more of the metrics listed in the first argument needs
-- to be sampled, the action is executed and the provided getter
-- functions will be used to extract the metric(s) from the action's
-- return value.
--
-- The registered action might be called from a different thread and
-- therefore needs to be thread-safe.
--
-- This function allows you to sample groups of metrics together. This
-- is useful if
--
-- * you need a consistent view of several metric or
--
-- * sampling the metrics together is more efficient.
--
-- For example, sampling GC statistics needs to be done atomically or
-- a GC might strike in the middle of sampling, rendering the values
-- incoherent. Sampling GC statistics is also more efficient if done
-- in \"bulk\", as the run-time system provides a function to sample all
-- GC statistics at once.
--
-- Note that sampling of the metrics is only atomic if the provided
-- action computes @a@ atomically (e.g. if @a@ is a record, the action
-- needs to compute its fields atomically if the sampling is to be
-- atomic.)
--
-- (Note: The @RegisterGroup@ constraint can safely be ignored.)
registerGroup ::
  (RegisterGroup xs) =>
  -- | Metric identifiers and getter functions
  SamplingGroup metrics env xs ->
  -- | Action to sample the metric group
  IO env ->
  -- | Registration action
  Registration metrics
registerGroup = registerGroup_ []

infixl 9 :>

-- | A group of metrics derived from the same sample, where the derived
-- metrics are known statically (up to their sampled value).
data
  SamplingGroup ::
    (Symbol -> Symbol -> MetricType -> Type -> Type) ->
    Type ->
    [Type] ->
    Type
  where
  -- | The empty sampling group
  SamplingGroup :: SamplingGroup metrics env '[]
  -- | Add a metric to a sampling group
  (:>) ::
    -- | Group to add to
    SamplingGroup metrics env xs ->
    -- | Metric class, Labels, Getter function
    ( metrics name help metricType labels,
      labels,
      env -> MetricValue metricType
    ) ->
    SamplingGroup metrics env (metrics name help metricType labels ': xs)

-- | Helper class for `registerGroup`.
class RegisterGroup (xs :: [Type]) where
  registerGroup_ ::
    -- | Processed metrics
    [ ( Internal.Name,
        Internal.Labels,
        Internal.Help,
        env -> Internal.Value
      )
    ] ->
    -- | Metrics to be processed
    SamplingGroup metrics env xs ->
    -- | Action to sample the metric group
    IO env ->
    Registration metrics

-- | Base case
instance RegisterGroup '[] where
  registerGroup_ getters SamplingGroup sample =
    let getterMap = Sample.fromList getters
     in Registration $ Internal.registerGroup getterMap sample

-- | Inductive case
instance
  ( RegisterGroup xs,
    ToMetricValue metricType,
    KnownSymbol name,
    KnownSymbol help,
    ToLabels labels
  ) =>
  RegisterGroup (metrics name help metricType labels ': xs)
  where
  registerGroup_ getters (group :> (_, labels, getter)) sample =
    let name = T.pack $ symbolVal (Proxy @name)
        labels' = toLabels labels
        help = T.pack $ symbolVal (Proxy @help)
        getter' =
          ( name,
            labels',
            help,
            toMetricValue (Proxy @metricType) . getter
          )
     in registerGroup_ (getter' : getters) group sample

-- | Like 'registerGroup', but where each metric may generate any number
-- of values at arbitrary labels.
--
-- This function is intended as an escape hatch, allowing one to bypass
-- the usual constraint on metric registrations: that the set of metric
-- identifiers (name + labels) to be emitted at sampling time is known
-- at registration time.
--
-- In exchange, you must ensure that the identifiers of the metrics you
-- emit using this functionality do not collide with other metric
-- identifiers, as no check for metric identifier collisions can be
-- performed at registration time. In the case of a collision at
-- sampling time, metrics /not/ registered via this function take
-- precedence; precedence is otherwise undefined.
--
-- (Note: The @RegisterUncheckedDynamicGroup@ constraint can safely be
-- ignored.)
registerUncheckedDynamicGroup ::
  (RegisterUncheckedDynamicGroup xs) =>
  -- | Metric identifiers and getter functions
  UncheckedDynamicSamplingGroup metrics env xs ->
  -- | Action to sample the metric group
  IO env ->
  -- | Registration action
  Registration metrics
registerUncheckedDynamicGroup = registerUncheckedDynamicGroup_ []

infixl 9 ::>

-- | A group of metrics derived from the same sample, where the labels of
-- the derived metrics are not known statically.
data
  UncheckedDynamicSamplingGroup ::
    (Symbol -> Symbol -> MetricType -> Type -> Type) ->
    Type ->
    [Type] ->
    Type
  where
  -- | The empty sampling group
  UncheckedDynamicSamplingGroup :: UncheckedDynamicSamplingGroup metrics env '[]
  -- | Add a metric to a sampling group
  (::>) ::
    -- | Group to add to
    UncheckedDynamicSamplingGroup metrics env xs ->
    -- | Metric class, Labels, Getter function
    ( metrics name help metricType labels,
      env -> [(labels, MetricValue metricType)] -- Not using Map to avoid requiring an `Ord` instance for `labels`
    ) ->
    UncheckedDynamicSamplingGroup metrics env (metrics name help metricType labels ': xs)

-- | Helper class for `registerUncheckedDynamicGroup`.
class RegisterUncheckedDynamicGroup (xs :: [Type]) where
  registerUncheckedDynamicGroup_ ::
    -- | Processed metrics
    [ ( Internal.Name,
        Internal.Help,
        env -> Map Internal.Labels Internal.Value
      )
    ] ->
    -- | Metrics to be processed
    UncheckedDynamicSamplingGroup metrics env xs ->
    -- | Action to sample the metric group
    IO env ->
    Registration metrics

-- | Base case
instance RegisterUncheckedDynamicGroup '[] where
  registerUncheckedDynamicGroup_ getters UncheckedDynamicSamplingGroup sample =
    let getterMap =
          Map.fromList $
            map
              (\(name, help, getter) -> (name, (help, getter)))
              getters
     in Registration $ Internal.registerUncheckedDynamicGroup getterMap sample

-- | Inductive case
instance
  ( RegisterUncheckedDynamicGroup xs,
    ToMetricValue metricType,
    KnownSymbol name,
    KnownSymbol help,
    ToLabels labels
  ) =>
  RegisterUncheckedDynamicGroup (metrics name help metricType labels ': xs)
  where
  registerUncheckedDynamicGroup_ getters (group ::> (_, getter)) sample =
    let name = T.pack $ symbolVal (Proxy @name)
        help = T.pack $ symbolVal (Proxy @help)
        getter' =
          ( name,
            help,
            Map.fromList
              . map (bimap toLabels (toMetricValue (Proxy @metricType)))
              . getter
          )
     in registerUncheckedDynamicGroup_ (getter' : getters) group sample

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
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  -- | Metric class
  metrics name help 'CounterType labels ->
  -- | Labels
  labels ->
  -- | Metric store
  Store metrics ->
  IO Counter.Counter
createCounter = createGeneric Internal.createCounter

-- | Create and permanently register a zero-initialized gauge.
--
-- Can throw 'RegistrationError' exceptions.
createGauge ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  -- | Metric class
  metrics name help 'GaugeType labels ->
  -- | Labels
  labels ->
  -- | Metric store
  Store metrics ->
  IO Gauge.Gauge
createGauge = createGeneric Internal.createGauge

-- | Create and permanently register an empty histogram. The buckets of
-- the histogram are fixed and defined by the given upper bounds.
--
-- Can throw 'RegistrationError' exceptions.
createHistogram ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  -- | Upper bounds of buckets
  [Histogram.UpperBound] ->
  -- | Metric class
  metrics name help 'HistogramType labels ->
  -- | Labels
  labels ->
  -- | Metric store
  Store metrics ->
  IO Histogram.Histogram
createHistogram upperBounds =
  createGeneric (Internal.createHistogram upperBounds)

createGeneric ::
  forall metrics name help metricType labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels) =>
  ( Internal.Identifier ->
    Internal.Help ->
    Internal.Store ->
    IO (MetricsImpl metricType)
  ) ->
  -- | Metric class
  metrics name help metricType labels ->
  -- | Labels
  labels ->
  -- | Metric store
  Store metrics ->
  IO (MetricsImpl metricType)
createGeneric f _ labels (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Internal.Identifier name (toLabels labels)
      help = T.pack $ symbolVal (Proxy @help)
   in f identifier help store

-- | Create a new, empty dynamic group of counters and permanently register it.
--
-- Can throw 'RegistrationError' exceptions.
createCounterVector ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels, Ord labels) =>
  -- | Metric class
  metrics name help 'CounterType labels ->
  -- | Metric store
  Store metrics ->
  -- | A function for transforming sampled metrics. Use `Map.toList` to specify a no-op transformation.
  (Map labels Double -> [(labels, Double)]) ->
  IO (CounterVector labels)
createCounterVector metricConstructor store f = do
  counterVector <- CounterVector.new
  registerPermanently store $
    let group =
          UncheckedDynamicSamplingGroup ::> (metricConstructor, f)
     in registerUncheckedDynamicGroup
          group
          (CounterVector.read counterVector)
  pure counterVector

-- | Create an empty dynamic group of gauges and permanently register it.
--
-- Can throw 'RegistrationError' exceptions.
createGaugeVector ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels, Ord labels) =>
  -- | Metric class
  metrics name help 'GaugeType labels ->
  -- | Metric store
  Store metrics ->
  -- | A function for transforming sampled metrics. Use `Map.toList` to specify a no-op transformation.
  (Map labels Double -> [(labels, Double)]) ->
  IO (GaugeVector labels)
createGaugeVector metricConstructor store f = do
  gaugeVector <- GaugeVector.new
  registerPermanently store $
    let group =
          UncheckedDynamicSamplingGroup ::> (metricConstructor, f)
     in registerUncheckedDynamicGroup
          group
          (GaugeVector.read gaugeVector)
  pure gaugeVector

-- | Create an empty dynamic group of histograms and permanently register it. The buckets of
-- the histogram are fixed and defined by the given upper bounds.
--
-- Can throw 'RegistrationError' exceptions.
createHistogramVector ::
  forall metrics name help labels.
  (KnownSymbol name, KnownSymbol help, ToLabels labels, Ord labels) =>
  -- | Upper bounds of buckets
  [Histogram.UpperBound] ->
  -- | Metric class
  metrics name help 'HistogramType labels ->
  -- | Metric store
  Store metrics ->
  -- | A function for transforming sampled metrics. Use `Map.toList` to specify a no-op transformation.
  (Map labels HistogramSample -> [(labels, HistogramSample)]) ->
  IO (HistogramVector labels)
createHistogramVector upperBounds metricConstructor store f = do
  histogramVector <- HistogramVector.new upperBounds
  registerPermanently store $
    let group =
          UncheckedDynamicSamplingGroup ::> (metricConstructor, f)
     in registerUncheckedDynamicGroup
          group
          (HistogramVector.read histogramVector)
  pure histogramVector

------------------------------------------------------------------------

-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store metrics -> IO Internal.Sample
sampleAll (Store store) = Internal.sampleAll store

------------------------------------------------------------------------

-- * Predefined metrics

-- | Register a number of metrics related to garbage collector
-- behavior.
--
-- To enable GC statistics collection, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.
registerGcMetrics :: Registration GcMetrics
registerGcMetrics = registerGroup samplingGroup getRTSStats
  where
    samplingGroup =
      SamplingGroup
        :> (Gcs, (), fromIntegral . Stats.gcs)
        :> (MajorGcs, (), fromIntegral . Stats.major_gcs)
        :> (AllocatedBytes, (), fromIntegral . Stats.allocated_bytes)
        :> (MaxLiveBytes, (), fromIntegral . Stats.max_live_bytes)
        :> (MaxLargeObjectsBytes, (), fromIntegral . Stats.max_large_objects_bytes)
        :> (MaxCompactBytes, (), fromIntegral . Stats.max_compact_bytes)
        :> (MaxSlopBytes, (), fromIntegral . Stats.max_slop_bytes)
        :> (MaxMemInUseBytes, (), fromIntegral . Stats.max_mem_in_use_bytes)
        :> (CumulativeLiveBytes, (), fromIntegral . Stats.cumulative_live_bytes)
        :> (CopiedBytes, (), fromIntegral . Stats.copied_bytes)
        :> (ParCopiedBytes, (), fromIntegral . Stats.par_copied_bytes)
        :> (CumulativeParMaxCopiedBytes, (), fromIntegral . Stats.cumulative_par_max_copied_bytes)
        :> (CumulativeParBalancedCopiedBytes, (), fromIntegral . Stats.cumulative_par_balanced_copied_bytes)
        :> (InitCpuNs, (), fromIntegral . Stats.init_cpu_ns)
        :> (InitElapsedNs, (), fromIntegral . Stats.init_elapsed_ns)
        :> (MutatorCpuNs, (), fromIntegral . Stats.mutator_cpu_ns)
        :> (MutatorElapsedNs, (), fromIntegral . Stats.mutator_elapsed_ns)
        :> (GcCpuNs, (), fromIntegral . Stats.gc_cpu_ns)
        :> (GcElapsedNs, (), fromIntegral . Stats.gc_elapsed_ns)
        :> (CpuNs, (), fromIntegral . Stats.cpu_ns)
        :> (ElapsedNs, (), fromIntegral . Stats.elapsed_ns)
        :> (NonmovingGcSyncCpuNs, (), fromIntegral . Stats.nonmoving_gc_sync_cpu_ns)
        :> (NonmovingGcSyncElapsedNs, (), fromIntegral . Stats.nonmoving_gc_sync_elapsed_ns)
        :> (NonmovingGcSyncMaxElapsedNs, (), fromIntegral . Stats.nonmoving_gc_sync_max_elapsed_ns)
        :> (NonmovingGcCpuNs, (), fromIntegral . Stats.nonmoving_gc_cpu_ns)
        :> (NonmovingGcElapsedNs, (), fromIntegral . Stats.nonmoving_gc_elapsed_ns)
        :> (NonmovingGcMaxElapsedNs, (), fromIntegral . Stats.nonmoving_gc_max_elapsed_ns)
        -- GCDetails
        :> (GcDetailsGen, (), fromIntegral . Stats.gcdetails_gen . Stats.gc)
        :> (GcDetailsThreads, (), fromIntegral . Stats.gcdetails_threads . Stats.gc)
        :> (GcDetailsAllocatedBytes, (), fromIntegral . Stats.gcdetails_allocated_bytes . Stats.gc)
        :> (GcDetailsLiveBytes, (), fromIntegral . Stats.gcdetails_live_bytes . Stats.gc)
        :> (GcDetailsLargeObjectsBytes, (), fromIntegral . Stats.gcdetails_large_objects_bytes . Stats.gc)
        :> (GcDetailsCompactBytes, (), fromIntegral . Stats.gcdetails_compact_bytes . Stats.gc)
        :> (GcDetailsSlopBytes, (), fromIntegral . Stats.gcdetails_slop_bytes . Stats.gc)
        :> (GcDetailsMemInUseBytes, (), fromIntegral . Stats.gcdetails_mem_in_use_bytes . Stats.gc)
        :> (GcDetailsCopiedBytes, (), fromIntegral . Stats.gcdetails_copied_bytes . Stats.gc)
        :> (GcDetailsParMaxCopiedBytes, (), fromIntegral . Stats.gcdetails_par_max_copied_bytes . Stats.gc)
        :> (GcDetailsParBalancedCopiedBytes, (), fromIntegral . Stats.gcdetails_par_balanced_copied_bytes . Stats.gc)
        :> (GcDetailsSyncElapsedNs, (), fromIntegral . Stats.gcdetails_sync_elapsed_ns . Stats.gc)
        :> (GcDetailsCpuNs, (), fromIntegral . Stats.gcdetails_cpu_ns . Stats.gc)
        :> (GcDetailsElapsedNs, (), fromIntegral . Stats.gcdetails_elapsed_ns . Stats.gc)
        :> (GcdetailsNonmovingGcSyncCpuNs, (), fromIntegral . Stats.gcdetails_nonmoving_gc_sync_cpu_ns . Stats.gc)
        :> (GcdetailsNonmovingGcSyncElapsedNs, (), fromIntegral . Stats.gcdetails_nonmoving_gc_sync_elapsed_ns . Stats.gc)

-- | Get RTS statistics.
getRTSStats :: IO Stats.RTSStats
getRTSStats = do
  enabled <- Stats.getRTSStatsEnabled
  if enabled
    then Stats.getRTSStats
    else return emptyRTSStats

-- | Empty RTS statistics, as if the application hasn't started yet.
emptyRTSStats :: Stats.RTSStats
emptyRTSStats =
  Stats.RTSStats
    { gcs = 0,
      major_gcs = 0,
      allocated_bytes = 0,
      max_live_bytes = 0,
      max_large_objects_bytes = 0,
      max_compact_bytes = 0,
      max_slop_bytes = 0,
      max_mem_in_use_bytes = 0,
      cumulative_live_bytes = 0,
      copied_bytes = 0,
      par_copied_bytes = 0,
      cumulative_par_max_copied_bytes = 0,
      cumulative_par_balanced_copied_bytes = 0,
      init_cpu_ns = 0,
      init_elapsed_ns = 0,
      mutator_cpu_ns = 0,
      mutator_elapsed_ns = 0,
      gc_cpu_ns = 0,
      gc_elapsed_ns = 0,
      cpu_ns = 0,
      elapsed_ns = 0,
      nonmoving_gc_sync_cpu_ns = 0,
      nonmoving_gc_sync_elapsed_ns = 0,
      nonmoving_gc_sync_max_elapsed_ns = 0,
      nonmoving_gc_cpu_ns = 0,
      nonmoving_gc_elapsed_ns = 0,
      nonmoving_gc_max_elapsed_ns = 0,
      gc = emptyGCDetails
    }

emptyGCDetails :: Stats.GCDetails
emptyGCDetails =
  Stats.GCDetails
    { gcdetails_gen = 0,
      gcdetails_threads = 0,
      gcdetails_allocated_bytes = 0,
      gcdetails_live_bytes = 0,
      gcdetails_large_objects_bytes = 0,
      gcdetails_compact_bytes = 0,
      gcdetails_slop_bytes = 0,
      gcdetails_mem_in_use_bytes = 0,
      gcdetails_copied_bytes = 0,
      gcdetails_par_max_copied_bytes = 0,
      gcdetails_par_balanced_copied_bytes = 0,
      gcdetails_sync_elapsed_ns = 0,
      gcdetails_cpu_ns = 0,
      gcdetails_elapsed_ns = 0,
      gcdetails_nonmoving_gc_sync_cpu_ns = 0,
      gcdetails_nonmoving_gc_sync_elapsed_ns = 0
    }

-- | The metrics registered by `registerGcMetrics`. These metrics are the
-- metrics exposed by the "GHC.Stats" module, listed in the same order for easy
-- comparison.

-- TODO: Fix metric names so that they conform to Prometheus conventions.
data GcMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
  Gcs ::
    GcMetrics
      "rts_gcs"
      "Total number of GCs"
      'CounterType
      ()
  MajorGcs ::
    GcMetrics
      "rts_major_gcs"
      "Total number of major (oldest generation) GCs"
      'CounterType
      ()
  AllocatedBytes ::
    GcMetrics
      "rts_allocated_bytes"
      "Total bytes allocated"
      'CounterType
      ()
  MaxLiveBytes ::
    GcMetrics
      "rts_max_live_bytes"
      "Maximum live data (including large objects + compact regions) in the heap. Updated after a major GC."
      'GaugeType
      ()
  MaxLargeObjectsBytes ::
    GcMetrics
      "rts_max_large_objects_bytes"
      "Maximum live data in large objects"
      'GaugeType
      ()
  MaxCompactBytes ::
    GcMetrics
      "rts_max_compact_bytes"
      "Maximum live data in compact regions"
      'GaugeType
      ()
  MaxSlopBytes ::
    GcMetrics
      "rts_max_slop_bytes"
      "Maximum slop"
      'GaugeType
      ()
  MaxMemInUseBytes ::
    GcMetrics
      "rts_max_mem_in_use_bytes"
      "Maximum memory in use by the RTS"
      'GaugeType
      ()
  CumulativeLiveBytes ::
    GcMetrics
      "rts_cumulative_live_bytes"
      "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program."
      'CounterType
      ()
  CopiedBytes ::
    GcMetrics
      "rts_copied_bytes"
      "Sum of copied_bytes across all GCs"
      'CounterType
      ()
  ParCopiedBytes ::
    GcMetrics
      "rts_par_copied_bytes"
      "Sum of copied_bytes across all parallel GCs"
      'GaugeType
      ()
  CumulativeParMaxCopiedBytes ::
    GcMetrics
      "rts_cumulative_par_max_copied_bytes"
      "Sum of par_max_copied_bytes across all parallel GCs. Deprecated."
      'GaugeType
      ()
  CumulativeParBalancedCopiedBytes ::
    GcMetrics
      "rts_cumulative_par_balanced_copied_bytes"
      "Sum of par_balanced_copied bytes across all parallel GCs"
      'GaugeType
      ()
  InitCpuNs ::
    GcMetrics
      "rts_init_cpu_ns"
      "Total CPU time used by the init phase @since 4.12.0.0"
      'CounterType
      ()
  InitElapsedNs ::
    GcMetrics
      "rts_init_elapsed_ns"
      "Total elapsed time used by the init phase @since 4.12.0.0"
      'CounterType
      ()
  MutatorCpuNs ::
    GcMetrics
      "rts_mutator_cpu_ns"
      "Total CPU time used by the mutator"
      'CounterType
      ()
  MutatorElapsedNs ::
    GcMetrics
      "rts_mutator_elapsed_ns"
      "Total elapsed time used by the mutator"
      'CounterType
      ()
  GcCpuNs ::
    GcMetrics
      "rts_gc_cpu_ns"
      "Total CPU time used by the GC"
      'CounterType
      ()
  GcElapsedNs ::
    GcMetrics
      "rts_gc_elapsed_ns"
      "Total elapsed time used by the GC"
      'CounterType
      ()
  CpuNs ::
    GcMetrics
      "rts_cpu_ns"
      "Total CPU time (at the previous GC)"
      'CounterType
      ()
  ElapsedNs ::
    GcMetrics
      "rts_elapsed_ns"
      "Total elapsed time (at the previous GC)"
      'CounterType
      ()
  NonmovingGcSyncCpuNs ::
    GcMetrics
      "nonmoving_gc_sync_cpu_ns"
      "The CPU time used during the post-mark pause phase of the concurrent nonmoving GC."
      'CounterType
      ()
  NonmovingGcSyncElapsedNs ::
    GcMetrics
      "nonmoving_gc_sync_elapsed_ns"
      "The time elapsed during the post-mark pause phase of the concurrent nonmoving GC."
      'CounterType
      ()
  NonmovingGcSyncMaxElapsedNs ::
    GcMetrics
      "nonmoving_gc_sync_max_elapsed_ns"
      "The maximum time elapsed during the post-mark pause phase of the concurrent nonmoving GC."
      'GaugeType
      ()
  NonmovingGcCpuNs ::
    GcMetrics
      "nonmoving_gc_cpu_ns"
      "The CPU time used during the post-mark pause phase of the concurrent nonmoving GC."
      'CounterType
      ()
  NonmovingGcElapsedNs ::
    GcMetrics
      "nonmoving_gc_elapsed_ns"
      "The time elapsed during the post-mark pause phase of the concurrent nonmoving GC."
      'CounterType
      ()
  NonmovingGcMaxElapsedNs ::
    GcMetrics
      "nonmoving_gc_max_elapsed_ns"
      "The maximum time elapsed during the post-mark pause phase of the concurrent nonmoving GC."
      'GaugeType
      ()
  -- GCDetails
  GcDetailsGen ::
    GcMetrics
      "rts_gc_gen"
      "The generation number of this GC"
      'GaugeType
      ()
  GcDetailsThreads ::
    GcMetrics
      "rts_gc_threads"
      "Number of threads used in this GC"
      'GaugeType
      ()
  GcDetailsAllocatedBytes ::
    GcMetrics
      "rts_gc_allocated_bytes"
      "Number of bytes allocated since the previous GC"
      'GaugeType
      ()
  GcDetailsLiveBytes ::
    GcMetrics
      "rts_gc_live_bytes"
      "Total amount of live data in the heap (incliudes large + compact data). Updated after every GC. Data in uncollected generations (in minor GCs) are considered live."
      'GaugeType
      ()
  GcDetailsLargeObjectsBytes ::
    GcMetrics
      "rts_gc_large_objects_bytes"
      "Total amount of live data in large objects"
      'GaugeType
      ()
  GcDetailsCompactBytes ::
    GcMetrics
      "rts_gc_compact_bytes"
      "Total amount of live data in compact regions"
      'GaugeType
      ()
  GcDetailsSlopBytes ::
    GcMetrics
      "rts_gc_slop_bytes"
      "Total amount of slop (wasted memory)"
      'GaugeType
      ()
  GcDetailsMemInUseBytes ::
    GcMetrics
      "rts_gc_mem_in_use_bytes"
      "Total amount of memory in use by the RTS"
      'GaugeType
      ()
  GcDetailsCopiedBytes ::
    GcMetrics
      "rts_gc_copied_bytes"
      "Total amount of data copied during this GC"
      'GaugeType
      ()
  GcDetailsParMaxCopiedBytes ::
    GcMetrics
      "rts_gc_par_max_copied_bytes"
      "In parallel GC, the max amount of data copied by any one thread. Deprecated."
      'GaugeType
      ()
  GcDetailsParBalancedCopiedBytes ::
    GcMetrics
      "rts_gc_par_balanced_copied_bytes"
      "In parallel GC, the amount of balanced data copied by all threads"
      'GaugeType
      ()
  GcDetailsSyncElapsedNs ::
    GcMetrics
      "rts_gc_sync_elapsed_ns"
      "The time elapsed during synchronisation before GC"
      'GaugeType
      ()
  GcDetailsCpuNs ::
    GcMetrics
      "rts_gc_cpu_ns"
      "The CPU time used during GC itself"
      'GaugeType
      ()
  GcDetailsElapsedNs ::
    GcMetrics
      "rts_gc_elapsed_ns"
      "The time elapsed during GC itself"
      'GaugeType
      ()
  GcdetailsNonmovingGcSyncCpuNs ::
    GcMetrics
      "gcdetails_nonmoving_gc_sync_cpu_ns"
      "The CPU time used during the post-mark pause phase of the concurrent nonmoving GC."
      'GaugeType
      ()
  GcdetailsNonmovingGcSyncElapsedNs ::
    GcMetrics
      "gcdetails_nonmoving_gc_sync_elapsed_ns"
      "The time elapsed during the post-mark pause phase of the concurrent nonmoving GC."
      'GaugeType
      ()
