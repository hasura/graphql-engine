{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-
This module is based on code from the prometheus-2.2.3 package:
https://hackage.haskell.org/package/prometheus-2.2.3

The license for prometheus-2.2.3 follows:

BSD 3-Clause License

Copyright (c) 2016-present, Bitnomial, Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

\* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

\* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

\* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
{-# HLINT ignore "Use tshow" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module defines an exporter for rendering metric 'Sample's into the
-- Prometheus format. To help ensure that the output of the exporter conforms
-- to the Prometheus format, the exporter peforms some sanitization of its
-- input.
module System.Metrics.Prometheus.Export
  ( sampleToPrometheus,
    escapeLabelValue,
  )
where

import Data.Bifunctor (second)
import qualified Data.ByteString.Builder as B
import qualified Data.HashMap.Strict as HashMap
import Data.Int (Int64)
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Metrics.Prometheus
  ( Sample,
    Value (Counter, Gauge, Histogram),
  )
import System.Metrics.Prometheus.Histogram
  ( HistogramSample (histBuckets, histCount, histSum),
  )
import System.Metrics.Prometheus.Internal.State
  ( Help,
    Labels,
    Name,
  )

------------------------------------------------------------------------------

-- | Encode a metrics 'Sample' into the Prometheus 2 exposition format.
--
-- This function does not validate its input; you must ensure that the
-- provided sample meets the following conditions:
--
-- 1. The names of metrics and labels must match the regex
-- @[a-zA-Z_][a-zA-Z0-9_]*@. See
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>.
--
-- 2. Within the values of labels, the backslash (@\\@), double-quote
-- (@\"@), and line feed (@\\n@) characters must be escaped as @\\\\@,
-- @\\\"@, and @\\n@, respectively. See 'escapeLabelValue'.
--
-- 3. If two metrics have the same name, they must also have the same
-- metric type.
--
--     * For each name, only metrics of one type (chosen arbitrarily)
--     will be retained while those of other types will be discarded.
--
-- You can assume that samples obtained directly from
-- 'System.Metrics.Prometheus.sampleAll' satisfy condition 1.
--
-- For example, a metrics sample consisting of
--
-- (1) a metric
--
--      * named @my_counter@
--      * with labels @{label_name_1="label value 1", label_name_2="label value 1"}@
--      * of type @Counter@
--      * with value @10@
--      * with help text @"Example counter"@
--
-- (2) a metric
--
--      * named @my_counter@
--      * with labels @{label_name_1="label value 2", label_name_2="label value 2"}@
--      * of type @Counter@
--      * with value @11@
--      * with help text @"Example counter"@
--
-- (3) a metric
--
--      * named @my_gauge@
--      * with no labels
--      * of type @Gauge@
--      * with value @100@
--      * with help text @"Example gauge"@
--
-- (4) a metric
--
--      * named @my_histogram@
--      * with labels @{label_name="label_value"}@
--      * of type @Histogram@
--      * with bucket upper bounds of @[1, 2, 3]@
--      * with observations @[1, 2, 3, 4]@
--      * with help text @"Example histogram"@
--
-- is encoded as follows:
--
-- > # HELP my_counter Example counter
-- > # TYPE my_counter counter
-- > my_counter{label_name_2="label value 1",label_name_1="label value 1"} 10.0
-- > my_counter{label_name_2="label value 2",label_name_1="label value 2"} 11.0
-- >
-- > # HELP my_gauge Example gauge
-- > # TYPE my_gauge gauge
-- > my_gauge 100.0
-- >
-- > # HELP my_histogram Example histogram
-- > # TYPE my_histogram histogram
-- > my_histogram_bucket{le="1.0",label_name="label_value"} 1
-- > my_histogram_bucket{le="2.0",label_name="label_value"} 2
-- > my_histogram_bucket{le="3.0",label_name="label_value"} 3
-- > my_histogram_bucket{le="+Inf",label_name="label_value"} 4
-- > my_histogram_sum{label_name="label_value"} 10.0
-- > my_histogram_count{label_name="label_value"} 4
sampleToPrometheus :: Sample -> B.Builder
sampleToPrometheus =
  mconcat
    . intersperse newline
    . map exportGroupedMetric
    . mapMaybe
      ( makeGroupedMetric
          . second (second M.toAscList)
      )
    . M.toAscList

------------------------------------------------------------------------------

data GroupedMetric
  = GroupedCounter Name Help [(Labels, Double)]
  | GroupedGauge Name Help [(Labels, Double)]
  | GroupedHistogram Name Help [(Labels, HistogramSample)]

-- Within a sample, metrics with the name should have the same metric
-- type, but this is not guaranteed. To handle the case where two
-- metrics have the same name but different metric types, we arbitrarily
-- choose, for each metric name, one of its metric types, and discard
-- all the metrics of that name that do not have that type.
--
makeGroupedMetric ::
  (Name, (Help, [(Labels, Value)])) -> Maybe GroupedMetric
makeGroupedMetric (metricName, (help, pairs@((_, headVal) : _))) =
  Just $
    case headVal of
      Counter _ ->
        GroupedCounter metricName help $
          flip mapMaybe pairs $ \(tags, val) ->
            sequence (tags, getCounterValue val)
      Gauge _ ->
        GroupedGauge metricName help $
          flip mapMaybe pairs $ \(tags, val) ->
            sequence (tags, getGaugeValue val)
      Histogram _ ->
        GroupedHistogram metricName help $
          flip mapMaybe pairs $ \(tags, val) ->
            sequence (tags, getHistogramValue val)
makeGroupedMetric _ =
  -- Discard empty groups so that we do not generate `# TYPE` and `#
  -- HELP` lines without any corresponding measurements. Such groups may
  -- be generated by the "unchecked dynamic groups" feature.
  Nothing

getCounterValue :: Value -> Maybe Double
getCounterValue = \case
  Counter x -> Just x
  _ -> Nothing

getGaugeValue :: Value -> Maybe Double
getGaugeValue = \case
  Gauge x -> Just x
  _ -> Nothing

getHistogramValue :: Value -> Maybe HistogramSample
getHistogramValue = \case
  Histogram hs -> Just hs
  _ -> Nothing

------------------------------------------------------------------------------

exportGroupedMetric :: GroupedMetric -> B.Builder
exportGroupedMetric = \case
  GroupedCounter metricName help labelsAndValues ->
    exportCounter metricName help labelsAndValues
  GroupedGauge metricName help labelsAndValues ->
    exportGauge metricName help labelsAndValues
  GroupedHistogram metricName help labelsAndValues ->
    exportHistogram metricName help labelsAndValues

-- Prometheus counter samples
exportCounter :: Name -> Help -> [(Labels, Double)] -> B.Builder
exportCounter metricName help labelsAndValues =
  mappend (metricHelpLine metricName help) $
    mappend (metricTypeLine "counter" metricName) $
      foldMap
        ( \(labels, value) ->
            metricSampleLine metricName labels (double value)
        )
        labelsAndValues

-- Prometheus gauge samples
exportGauge :: Name -> Help -> [(Labels, Double)] -> B.Builder
exportGauge metricName help labelsAndValues =
  mappend (metricHelpLine metricName help) $
    mappend (metricTypeLine "gauge" metricName) $
      foldMap
        ( \(labels, value) ->
            metricSampleLine metricName labels (double value)
        )
        labelsAndValues

-- Prometheus histogram samples
exportHistogram ::
  Name -> Help -> [(Labels, HistogramSample)] -> B.Builder
exportHistogram metricName help labelsAndValues =
  mappend (metricHelpLine metricName help) $
    mappend (metricTypeLine "histogram" metricName) $
      flip foldMap labelsAndValues $ \(labels, histSample) ->
        mconcat
          [ let cumulativeBuckets =
                  snd $ M.mapAccum cumulativeSum 0 (histBuckets histSample)
                  where
                    cumulativeSum !sum_ x = let z = sum_ + x in (z, z)
             in flip foldMap (M.toList cumulativeBuckets) $
                  \(upperBound, count) ->
                    metricSampleLine
                      metricName_bucket
                      (HashMap.insert "le" (T.pack (show upperBound)) labels)
                      (int count),
            metricSampleLine
              metricName_bucket
              (HashMap.insert "le" "+Inf" labels)
              (int (histCount histSample)),
            metricSampleLine
              (metricName <> "_sum")
              labels
              (double (histSum histSample)),
            metricSampleLine
              (metricName <> "_count")
              labels
              (int (histCount histSample))
          ]
  where
    metricName_bucket = metricName <> "_bucket"

------------------------------------------------------------------------------

-- Prometheus metric help line
metricHelpLine :: T.Text -> T.Text -> B.Builder
metricHelpLine metricName help
  | T.null help = mempty
  | otherwise =
      "# HELP "
        <> text metricName
        <> B.charUtf8 ' '
        <> text help
        <> newline

-- Prometheus metric type line
metricTypeLine :: B.Builder -> T.Text -> B.Builder
metricTypeLine metricType metricName =
  "# TYPE "
    <> text metricName
    <> B.charUtf8 ' '
    <> metricType
    <> newline

-- Prometheus metric sample line
metricSampleLine ::
  T.Text -> HashMap.HashMap T.Text T.Text -> B.Builder -> B.Builder
metricSampleLine metricName labels value =
  text metricName
    <> labelSet labels
    <> B.charUtf8 ' '
    <> value
    <> newline

-- Prometheus label set
labelSet :: HashMap.HashMap T.Text T.Text -> B.Builder
labelSet labels
  | HashMap.null labels = mempty
  | otherwise =
      let labelList =
            mconcat $
              intersperse (B.charUtf8 ',') $
                map labelPair $
                  HashMap.toList labels
       in B.charUtf8 '{'
            <> labelList
            <> B.charUtf8 '}'

-- Prometheus name-value label pair
labelPair :: (T.Text, T.Text) -> B.Builder
labelPair (labelName, labelValue) =
  text labelName
    <> B.charUtf8 '='
    <> B.charUtf8 '"'
    <> text labelValue
    <> B.charUtf8 '"'

------------------------------------------------------------------------------
-- Input sanitization

-- | A convenience function for escaping the backslash (@\\@),
-- double-quote (@\"@), and line feed (@\\n@) characters of a string as
-- @\\\\@, @\\\"@, and @\\n@, respectively, so that it is a valid
-- Prometheus label value. This function is not idempotent.
--
-- See
-- <https://prometheus.io/docs/instrumenting/exposition_formats/#comments-help-text-and-type-information>.
--
-- > >>> putStrLn $ escapeLabelValue "\n \" \\"
-- > \n \" \\

-- Implementation note: We do not apply this function on behalf of the
-- user because it is not idempotent.
escapeLabelValue :: T.Text -> T.Text
escapeLabelValue = T.concatMap escapeLabelValueChar

escapeLabelValueChar :: Char -> T.Text
escapeLabelValueChar c
  | c == '\n' = "\\n"
  | c == '"' = "\\\""
  | c == '\\' = "\\\\"
  | otherwise = T.singleton c

------------------------------------------------------------------------------
-- Builder helpers

text :: T.Text -> B.Builder
text = B.byteString . T.encodeUtf8

double :: Double -> B.Builder
double = B.stringUtf8 . show

int :: Int64 -> B.Builder
int = B.stringUtf8 . show

newline :: B.Builder
newline = B.charUtf8 '\n'
