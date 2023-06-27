NOTE: This tutorial is not being maintained. It needs to be rewritten.

# Tutorial

This document introduces the `ekg-prometheus` Prometheus client library,
and illustrates how to use the library to instrument your programs with
Prometheus metrics. If you are new to the library, read this document
first. If you have used the `ekg-core` library, on which
`ekg-prometheus` is based, you should still read this document first.
For a more complete API reference, see the Haddocks of the
`System.Metrics.Prometheus` module.

This document is a literate Haskell program:

```haskell
-- Note: The code in this tutorial is not being maintained.
main :: IO ()
main = pure ()
```

```haskell
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}
-- 
-- module Main where
-- 
-- import Control.Exception (assert)
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Map.Strict as M
-- import qualified Data.Text as T
-- import Data.Kind (Type)
-- import GHC.Generics (Generic)
-- import GHC.Stats (RTSStats (..), getRTSStats)
-- import GHC.TypeLits (Symbol)
-- 
-- -- This package's modules
-- import System.Metrics.Prometheus
-- import qualified System.Metrics.Prometheus.Counter as Counter
-- import qualified System.Metrics.Prometheus.Gauge as Gauge
```

Although you will need to use some type-level features of Haskell when
using the `ekg-prometheus` API, you will not need a solid understanding
of type-level programming. You can use `ekg-prometheus` proficiently
just by copying the examples presented in this tutorial.

For those who have used the original `ekg-core` library, Hasura's fork
adds the following features:

* dimensional/tagged metrics (Prometheus labels), and
* dynamic metrics (the ability to deregister and reregister metrics).

## Overview

Metrics are used to monitor program behavior and performance. All
metrics have:

- a name,
- a set of labels (possibly empty), and
- a way to get the metric's current value.

`ekg-prometheus` provides a way to register metrics in a global "metric
store". The store can then be used to get a snapshot of all metrics. The
store also serves as a central place to keep track of all the program's
metrics, both user and library defined.

This tutorial will show you how to:

- specify metrics,
- register and sample metrics,
- add labels to metrics,
- deregister metrics,
- use pre-defined metrics, and
- sample a subset of metrics atomically.

## Specifying metrics

Before you can register metrics to a metric store, you must first
specify _which_ metrics may be registered to that store.
`ekg-prometheus` will statically ensure that your specifications are
respected.

Your **metrics specification** must be given as a generalized algebraic
data type (GADT) with a specific kind signature. Here is an example GADT
that specifies two metrics:

```haskell
-- data AppMetrics1
--   :: Symbol -- ^ Metric name
--   -> Symbol -- ^ Metric documentation
--   -> MetricType -- ^ e.g. Counter, Gauge
--   -> Type -- ^ Label set structure
--   -> Type
--   where
--   Requests :: AppMetrics1 "app_requests" "" 'CounterType ()
--   Connections :: AppMetrics1 "app_connections" "" 'GaugeType ()
```

The `AppMetrics1` GADT has two constructors, `Requests` and
`Connections`, each of which correspond to a metric. The type parameters
of each constructor determine the name, type, and "label structure" of
their corresponding metric. For example, the `Requests` constructor
specifies a metric with:

* name "app_requests", and
* type counter, and
* labels disabled.

Tutorial note: We have glossed over labels for now, but will introduce
them properly later.

## Registering and sampling metrics

Now that you have created a metrics specification, you can use it to
annotate a metric store and start registering and collecting metrics.

Here is an example program that uses the above specification:

```haskell
-- app1 :: IO ()
-- app1 = do
--   -- Create a mutable reference to a metric store.
--   store <- newStore @AppMetrics1 -- (1)
-- 
--   -- Initialize mutable references to metrics.
--   requestsCounter <- Counter.new
--   connectionsGauge <- Gauge.new
-- 
--   -- Register the metrics to the metric store.
--   _ <- register store $ -- (2)
--     registerCounter Requests () (Counter.read requestsCounter) <>
--     registerGauge Connections () (Gauge.read connectionsGauge)
-- 
--   -- Update the values of the metrics.
--   Counter.inc requestsCounter
--   Gauge.set connectionsGauge 99
-- 
--   -- Get the current values of all the metrics in the store.
--   sample <- sampleAll store -- (3)
-- 
--   -- Verify the sample, just for this tutorial.
--   let expectedSample = M.fromList
--         [ ("app_requests", ("", M.singleton HM.empty (Counter 1)))
--         , ("app_connections", ("", M.singleton HM.empty (Gauge 99)))
--         ]
--   assert (sample == expectedSample) $ pure ()
```

1. Metric store references are parameterized by a metrics specification.
   In this case, we have used `-XTypeApplications` to explicitly name
   the intended metrics specification, even though GHC could infer the
   metrics specification itself.

1. The `register` IO action atomically applies a sequence of
   "registrations" to a metric store. Individual registrations are
   created by functions like `registerCounter` and `registerGauge`, and
   can be combined into a sequence of registrations by their `Semigroup`
   operation `<>`.

   The `registerCounter` function takes as its first argument a
   constructor of a metrics specification GADT. This constructor must
   have metric type `'CounterType`. Its second parameter specifies the
   set of "labels" to attach to the metric -- for now, labels have been
   disallowed. Its third parameter specifies the IO action that the
   store should use to sample the current value of the metric.

   The `registerGauge` function is the analogue of `registerCounter` for
   the gauge metric type.

1. The `sampleAll` function iterates through all of the metrics
   registered to the store, runs their sampling actions in turn, and
   collects the results. Note that sampling is _not_ atomic: While each
   metric will be retrieved atomically, the sample is not an atomic
   snapshot of the system as a whole.
   (For more information, see
   [sampling metrics atomically](#Sampling-groups-of-metrics-atomically))

## Adding labels to metrics

`ekg-prometheus` has a multi-dimensional data model, like
[Prometheus](https://prometheus.io). In this data model, metrics may be
annotated by a **labels set**,
which is a set of key-value pairs called **labels**.
Labels are useful for convenient filtering and aggregation of metric
data. In `ekg-prometheus`, metrics are identified by both their name
_and_ their label set, so metrics with the same name but different label
sets are distinct and independent metrics. When working with labelled
metrics, the constructors of a metrics specification GADT corrrespond to
**classes** of metrics that share the same name.

`ekg-prometheus` also has support for _structuring_ the representation
of your labels. A label set can be represented by a value of any type,
as long as the type is associated with a function that "renders" the
value into a label set. More specifically, a label set can be
represented by a value of any type that is an instance of the `ToLabels`
typeclass, which has a single function `toLabels :: ToLabels a => a ->
HashMap Text Text`.

Here is an example metrics specification that defines some labelled
metrics:

```haskell
-- data AppMetrics2
--   :: Symbol
--   -> Symbol
--   -> MetricType
--   -> Type -- ^ Label set structure
--   -> Type
--   where
--   -- (1)
--   HTTPRequests ::
--     AppMetrics2 "requests" "" 'CounterType EndpointLabels
--   DBConnections ::
--     AppMetrics2 "total_connections" "" 'GaugeType DataSourceLabels
-- 
-- -- (2)
-- newtype EndpointLabels = EndpointLabels { endpoint :: T.Text }
-- 
-- instance ToLabels EndpointLabels where
--   toLabels (EndpointLabels endpoint') = HM.singleton "endpoint" endpoint'
-- 
-- -- 3
-- data DataSourceLabels = DataSourceLabels
--   { source_name :: T.Text
--   , conn_info :: T.Text
--   } deriving (Generic)
-- instance ToLabels DataSourceLabels
```

1. The third type parameter of the constructors is used to specify
   label set structure.

   In this example, the types provided for the label set structure
   parameter are two user-defined types, `EndpointLabels` and
   `DataSourceLabels`.

1. Here, the `ToLabels` instance of `EndpointLabels` has been specified by
   hand.

1. Here, the `ToLabels` instance of `DataSourceLabels` has been specified
   using GHC.Generics.

    A `ToLabels` instance may be derived via GHC.Generics for any record
    that exclusively has fields of type `Text`. The record field names
    are used as the label keys.

Here is an example program using this metrics specification:

```haskell
-- app2 :: IO ()
-- app2 = do
--   store <- newStore @AppMetrics2
-- 
--   harpsichordRequests <- Counter.new
--   tablaRequests <- Counter.new
--   dbConnections <- Gauge.new
-- 
--   _ <- register store $ mconcat
--     [ registerCounter HTTPRequests (EndpointLabels "dev/harpsichord") (Counter.read harpsichordRequests)
--     , registerCounter HTTPRequests (EndpointLabels "dev/tabla") (Counter.read tablaRequests)
--     , let labels = DataSourceLabels
--             { source_name = "myDB"
--             , conn_info = "localhost:5432" }
--       in  registerGauge DBConnections labels (Gauge.read dbConnections)
--     ]
-- 
--   Counter.inc tablaRequests
--   Gauge.set dbConnections 99
-- 
--   sample <- sampleAll store
-- 
--   let expectedSample = M.fromList
--         [ ( "requests"
--           , ( ""
--             , M.fromList
--                 [ (HM.singleton "endpoint" "dev/harpsichord", Counter 0)
--                 , (HM.singleton "endpoint" "dev/tabla", Counter 1)
--                 ]
--             )
--           )
--         , ( "total_connections"
--           , ( ""
--             , M.singleton
--                 ( HM.fromList
--                   [ ("source_name", "myDB")
--                   , ("conn_info", "localhost:5432")
--                   ]
--                 )
--                 (Gauge 99)
--             )
--           )
--         ]
--   assert (sample == expectedSample) $ pure ()
```

## Reregistering and deregistering metrics

Metrics you register to a metric store need not be permanent; metrics
can be replaced (reregistered) or removed (deregistered).

Reregistering metrics in `ekg-prometheus` is implicit. If you try to
register a metric at a (name, label set) pair that is already in use by
an existing metric, the existing metric will be deregistered and
replaced with the new metric.

Deregistering metrics in `ekg-prometheus` is explicit, and is done using
**deregistration handles**. When you register a set of metrics with
`register`, `register` will return an IO action (the deregistration
handle) that can be used to _specifically_ deregister the newly
registered metrics. This action is specific in the following sense: if a
deregistration handle targets a metric, and that metric is replaced by a
new metric, the new metric will not be deregistered if the handle is
used.

Here is an example program that illustrates the reregistration and
deregistration of metrics:

```haskell
-- app3 :: IO ()
-- app3 = do
--   store <- newStore @AppMetrics1 -- reusing a previous specification
-- 
--   requestsCounter <- Counter.new
--   connectionsGauge <- Gauge.new
-- 
--   -- Register the metrics, retaining the deregistration handle. -- (1)
--   deregistrationHandle <- register store $
--     registerCounter Requests () (Counter.read requestsCounter) <>
--     registerGauge Connections () (Gauge.read connectionsGauge)
-- 
--   Counter.inc requestsCounter
--   Gauge.set connectionsGauge 99
-- 
--   sample1 <- sampleAll store
--   let expectedSample1 = M.fromList
--         [ ("app_requests", ("", M.singleton HM.empty (Counter 1)))
--         , ("app_connections", ("", M.singleton HM.empty (Gauge 99)))
--         ]
--   assert (sample1 == expectedSample1) $ pure ()
-- 
--   -- Replace (reregister) the connections gauge metric with a new one.
--   replacementConnectionsGauge <- Gauge.new
--   Gauge.set replacementConnectionsGauge 5
--   _ <- register store $
--     registerGauge Connections () (Gauge.read replacementConnectionsGauge)
-- 
--   sample2 <- sampleAll store
--   let expectedSample2 = M.fromList
--         [ ("app_requests", ("", M.singleton HM.empty (Counter 1)))
--         , ("app_connections", ("", M.singleton HM.empty (Gauge 5)))
--         ]
--   assert (sample2 == expectedSample2) $ pure ()
-- 
--   -- Use the deregistration handle to deregister the original metrics.
--   deregistrationHandle -- (2)
-- 
--   sample3 <- sampleAll store
--   let expectedSample3 =
--         M.singleton "app_connections" $
--           ("", M.singleton HM.empty (Gauge 5))
--   assert (sample3 == expectedSample3) $ pure ()
```

1. Deregistration handles were present in in all previous examples,
   but we ignored them for simplicity.

1. The deregistration handle removes all metrics registered by the
   initial call to `register`. In particular, this does not include the
   reregistered gauge.

## Using pre-defined sets of metrics

Other libraries can define sets of metrics that you can register to your
metric store. For example, the `ekg-prometheus` library defines metrics
for the runtime system metrics exposed by `GHC.Stats` -- see
`registerGcMetrics`. Libraries that define metrics must also define
their own metrics specifications, which you will need to include in your
own metrics specification in order to use their metrics.

Here is an example program which includes the `GcMetrics` metrics
specification (used by `registerGcMetrics`) as a part of another metrics
specification:

```haskell
-- data AppMetrics4 :: Symbol -> Symbol -> MetricType -> Type -> Type where
--   -- (1)
--   GcSubset ::
--     GcMetrics name help metricType labels ->
--     AppMetrics4 name help metricType labels
-- 
-- app4 :: IO ()
-- app4 = do
--   store <- newStore @AppMetrics4
--   -- (2)
--   _ <- register (subset GcSubset store) registerGcMetrics
--   pure ()
```

1. We define a constructor, `GcSubset`, that takes any metric class from
   `GcMetrics` and makes it a metric class of `AppMetrics4`.

    Metric classes with the same type parameters (name, metric type, and
    label structure) are treated in the same way by all functions of
    `ekg-prometheus`, so it is enough for our constructor to "forward"
    the type parameters.

1. In order use `registerGcMetrics` with our metric store, we must use
   the `subset` function to create a new reference to our metric store
   restricted to the `GcMetrics` metrics specification that
   `registerGcMetrics` expects.

## Sampling groups of metrics atomically

`ekg-prometheus` provides a way to obtain atomic snapshots of a group of
metrics. This can be useful if

- you need a consistent view of several metrics, or
- sampling the metrics together is more efficient.

For example, sampling GC statistics needs to be done atomically or a GC
might strike in the middle of sampling, rendering the values incoherent.
Sampling GC statistics is also more efficient if done in "bulk", as the
run-time system provides a function to sample all GC statistics at once.

The usual metric samples obtained through the `sampleAll` function are
generally _not_ atomic snapshots of their metrics. In general, because
metric sampling actions can be arbitrary `IO` actions, `ekg-prometheus`
has no way to ensure that independent metrics are sampled atomically.

However, a group of metrics can be sampled atomically if

- their values are all derived from a single shared value, via pure
  functions, and
- the IO action that computes the shared value does so atomically (e.g.
  if the shared value is a record, the action needs to compute its
  fields atomically).

To register an atomically-sampled group of metrics, use the
`registerGroup` function and the `SamplingGroup` type. Here is an
example program that does this:

```haskell
-- -- (1)
-- data GcMetrics' :: Symbol -> Symbol -> MetricType -> Type -> Type where
--   Gcs' :: GcMetrics' "rts_gcs" "" 'CounterType ()
--   MaxLiveBytes' :: GcMetrics' "rts_max_live_bytes" "" 'GaugeType ()
-- 
-- app5 :: IO ()
-- app5 = do
--   store <- newStore @GcMetrics'
-- 
--   -- (2)
--   let samplingGroup =
--         SamplingGroup
--           :> (Gcs', (), fromIntegral . gcs)
--           :> (MaxLiveBytes', (), fromIntegral . max_live_bytes)
-- 
--   _ <- register store $
--         registerGroup samplingGroup getRTSStats -- (3)
--   pure ()
```

1. We replicate part of the `GcMetrics` metrics specification from
   `ekg-prometheus`.

1. We create a sampling group of two of the runtime system metrics.

    Each metric is represented by:
    - a metric class,
    - a label set, and
    - a pure function that computes the metric's value from a single
      value that is shared with all metrics of the sampling group.

1. We use the `registerGroup` function to pair our sampling group with
   an IO action, `getRTSStats`, that produces the shared value.

## Conclusion

This tutorial introduced and demonstrated the core features of the
`ekg-prometheus` library:

- specifying metrics,
- registering and sampling metrics,
- labelling metrics,
- deregistering metrics,
- using pre-defined metrics, and
- sampling a subset of metrics atomically.

Additional features and details can be found in the following documents:

- the Haddocks for the `System.Metrics.Prometheus` module
- the Appendix section below.

## Appendix

This section contains extra material that is not needed to use the
`ekg-prometheus` library, but may be useful. This section assumes an
understanding of the material covered in the tutorial.

### Simulating static metrics

You can register metrics to a metric store so that they cannot be
removed or modified. Here is an example program that does this.

```haskell
-- -- (1)
-- data AppMetrics6 :: Symbol -> Symbol -> MetricType -> Type -> Type where
--   DynamicSubset ::
--     DynamicMetrics name help metricType labels ->
--     AppMetrics6 name help metricType labels
--   StaticSubset ::
--     StaticMetrics name help metricType labels ->
--     AppMetrics6 name help metricType labels
-- 
-- data StaticMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
--   MyStaticMetric :: StaticMetrics "my_static_metric" "" 'CounterType ()
-- 
-- data DynamicMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type where
--   MyDynamicMetric :: DynamicMetrics "my_dynamic_metric" "" 'CounterType ()
-- 
-- app6 :: IO ()
-- app6 = do
--   (_store, _staticMetrics) <- do
--     store <- newStore @AppMetrics6
--     -- (2)
--     let staticRef = subset StaticSubset store
--         dynamicRef = subset DynamicSubset store
--     staticMetrics <- registerStaticMetrics staticRef
--     pure (dynamicRef, staticMetrics)
-- 
--   -- (3)
--   pure ()
-- 
-- registerStaticMetrics :: Store StaticMetrics -> IO Counter.Counter
-- registerStaticMetrics store = do
--   counter <- Counter.new
--   _ <- register store $
--         registerCounter MyStaticMetric () (Counter.read counter)
--   pure counter
```

1. We divide our metrics specification into two subsets: one for static
   metrics that should not be removed or modified after being
   registered, and the other for dynamic metrics that may need to be
   removed or modified.

1. We use the `subset` function twice to create restricted references to
   the metric store. The first reference is scoped to the static subset,
   which we use to register the static metrics. The second reference is
   scoped to the dynamic subset, and is the only reference to the metric
   store that we expose.

1. At this point, the only reference to the store is scoped to the
   subset of dynamic metrics. There is no way to register or deregister
   metrics from the static subset, making those metrics effectively
   immutable.

## Tutorial verification

This tutorial is compiled and run as a test using the `markdown-unlit`
package.

```haskell
-- main :: IO ()
-- main = do
--   app1
--   app2
--   app3
--   app4
--   app5
--   app6
```
