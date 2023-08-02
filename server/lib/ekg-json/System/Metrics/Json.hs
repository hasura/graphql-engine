{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines an encoding of ekg metrics as JSON. This
-- encoding is used by the ekg web UI. This encoding is also
-- standardized so that other web servers and frameworks can also expose
-- ekg metrics.
module System.Metrics.Json
  ( -- * Converting metrics to JSON values
    sampleToJson,
    valueToJson,

    -- ** Newtype wrappers with instances
    Sample (..),
    Value (..),
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------

-- * Converting metrics to JSON values

-- | Encode metrics as nested JSON objects. Each "." in the metric name
-- introduces a new level of nesting.
--
-- For example, a set of metrics consisting of
--
-- (1) a metric named @foo.counter@, with tags @{key1:"val1"}@, of type
-- @Counter@, and with value @10@;
-- (2) a metric named @foo.counter@, with tags @{key1:"val2"}@, of type
-- @Counter@, and with value @11@;
-- (3) a metric named @foo.distribution@, with no tags, of type
-- @Distribution@, and with value
-- @Distribution{count=1, max=1, mean=1, min=1, sum=1, variance=0}@;
-- (4) a metric named @gauge@, with no tags, of type @Gauge@, and with
-- value @100@; and
-- (5) a metric named @label@, with no tags, of type @Label@, and with
-- value @"bar"@
--
-- is encoded as
--
-- > {
-- >   "foo": {
-- >     "counter": [
-- >       {
-- >         "tags": {
-- >           "key1": "val2"
-- >         },
-- >         "value": {
-- >           "type": "c",
-- >           "val": 11
-- >         }
-- >       },
-- >       {
-- >         "tags": {
-- >           "key1": "val1"
-- >         },
-- >         "value": {
-- >           "type": "c",
-- >           "val": 10
-- >         }
-- >       }
-- >     ],
-- >     "distribution": [
-- >       {
-- >         "tags": {},
-- >         "value": {
-- >           "count": 1,
-- >           "max": 1,
-- >           "mean": 1,
-- >           "min": 1,
-- >           "sum": 1,
-- >           "type": "d",
-- >           "variance": 0
-- >         }
-- >       }
-- >     ]
-- >   },
-- >   "gauge": [
-- >     {
-- >       "tags": {},
-- >       "value": {
-- >         "type": "g",
-- >         "val": 100
-- >       }
-- >     }
-- >   ],
-- >   "label": [
-- >     {
-- >       "tags": {},
-- >       "value": {
-- >         "type": "l",
-- >         "val": "bar"
-- >       }
-- >     }
-- >   ]
-- > }
sampleToJson :: Metrics.Sample -> A.Value
sampleToJson metrics =
  HashMap.foldlWithKey' build A.emptyObject (groupMetrics metrics)
  where
    -- Group a set of metrics by metric name.
    groupMetrics ::
      HashMap.HashMap Metrics.Identifier Metrics.Value ->
      HashMap.HashMap T.Text [(HashMap.HashMap T.Text T.Text, Metrics.Value)]
    groupMetrics = HashMap.foldlWithKey' f HashMap.empty
      where
        f m (Metrics.Identifier name tags) value =
          let !x = (tags, value)
           in -- Info: If inserting at an existing key,
              -- `Data.HashMap.Strict.insertWith f key value` calls
              -- `f value existingValue`.
              HashMap.insertWith (++) name [x] m

    build ::
      A.Value ->
      T.Text ->
      [(HashMap.HashMap T.Text T.Text, Metrics.Value)] ->
      A.Value
    build json name values = go json (T.splitOn "." name)
      where
        valuesArray :: A.Value
        valuesArray = A.Array $ V.fromList $ map taggedValueToJson values

        taggedValueToJson ::
          (HashMap.HashMap T.Text T.Text, Metrics.Value) -> A.Value
        taggedValueToJson (tags, value) =
          A.object
            [ "tags" .= tags,
              "value" .= valueToJson value
            ]

        go :: A.Value -> [T.Text] -> A.Value
        go (A.Object m) (str : rest) = A.Object $ insert key goRest m
          where
            goRest = case rest of
              [] -> valuesArray
              (_ : _) -> go (fromMaybe A.emptyObject $ lookup_ key m) rest
            key = K.fromText str
            insert = KM.insert
            lookup_ = KM.lookup
        go v _ = typeMismatch "Object" v

typeMismatch :: String -> A.Value -> a
typeMismatch expected actual =
  error $
    "when expecting a "
      ++ expected
      ++ ", encountered "
      ++ name
      ++ " instead"
  where
    name = case actual of
      A.Object _ -> "Object"
      A.Array _ -> "Array"
      A.String _ -> "String"
      A.Number _ -> "Number"
      A.Bool _ -> "Boolean"
      A.Null -> "Null"

-- | Encodes a single metric as a JSON object. For example:
--
-- >>> valueToJson (Counter 89460)
-- Object (fromList [("type",String "c"),("val",Number 89460.0)])
-- -- { "type": "c", "val": 89460 }
--
-- (To prevent any possible confusion, the input is of type
-- 'System.Metrics.Value' from "System.Metrics", and the output is of
-- type 'Data.Aeson.Value' from "Data.Aeson".)
valueToJson :: Metrics.Value -> A.Value
valueToJson (Metrics.Counter n) = scalarToJson n CounterType
valueToJson (Metrics.Gauge n) = scalarToJson n GaugeType
valueToJson (Metrics.Label l) = scalarToJson l LabelType
valueToJson (Metrics.Distribution l) = distrubtionToJson l

-- | Convert a scalar metric (i.e. counter, gauge, or label) to a JSON
-- value.
scalarToJson :: (A.ToJSON a) => a -> MetricType -> A.Value
scalarToJson val ty =
  A.object
    ["val" .= val, "type" .= metricType ty]
{-# SPECIALIZE scalarToJson :: Int64 -> MetricType -> A.Value #-}
{-# SPECIALIZE scalarToJson :: T.Text -> MetricType -> A.Value #-}

data MetricType
  = CounterType
  | GaugeType
  | LabelType
  | DistributionType

metricType :: MetricType -> T.Text
metricType CounterType = "c"
metricType GaugeType = "g"
metricType LabelType = "l"
metricType DistributionType = "d"

-- | Convert a distribution to a JSON value.
distrubtionToJson :: Distribution.Stats -> A.Value
distrubtionToJson stats =
  A.object
    [ "mean" .= Distribution.mean stats,
      "variance" .= Distribution.variance stats,
      "count" .= Distribution.count stats,
      "sum" .= Distribution.sum stats,
      "min" .= Distribution.min stats,
      "max" .= Distribution.max stats,
      "type" .= metricType DistributionType
    ]

------------------------------------------------------------------------

-- ** Newtype wrappers with instances

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Sample' without creating an orphan instance.
newtype Sample = Sample Metrics.Sample
  deriving (Show)

-- | Uses 'sampleToJson'.
instance A.ToJSON Sample where
  toJSON (Sample s) = sampleToJson s

-- | Newtype wrapper that provides a 'A.ToJSON' instances for the
-- underlying 'Metrics.Value' without creating an orphan instance.
newtype Value = Value Metrics.Value
  deriving (Show)

-- | Uses 'valueToJson'.
instance A.ToJSON Value where
  toJSON (Value v) = valueToJson v
