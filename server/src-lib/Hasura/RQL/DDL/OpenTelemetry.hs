module Hasura.RQL.DDL.OpenTelemetry
  ( runSetOpenTelemetryConfig,
    runSetOpenTelemetryStatus,
    parseOtelExporterConfig,
    parseOtelBatchSpanProcessorConfig,
  )
where

import Control.Lens ((.~))
import Data.Bifunctor (first)
import Data.Environment (Environment)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hasura.Base.Error (Code (InvalidParams), QErr, err400)
import Hasura.EncJSON
import Hasura.Metadata.Class ()
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Headers (makeHeadersFromConf)
import Hasura.RQL.Types.Common (successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.OpenTelemetry
import Hasura.RQL.Types.SchemaCache.Build
import Network.HTTP.Client (Request (requestHeaders), requestFromURI)
import Network.URI (parseURI)

-- | Set the OpenTelemetry configuration to the provided value.
runSetOpenTelemetryConfig ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  OpenTelemetryConfig ->
  m EncJSON
runSetOpenTelemetryConfig otelConfig = do
  withNewInconsistentObjsCheck
    $ buildSchemaCacheFor (MOOpenTelemetry OtelSubobjectAll)
    $ MetadataModifier
    $ metaOpenTelemetryConfig
    .~ otelConfig
  pure successMsg

-- | Set just the "status" field of the OpenTelemetry configuration.
runSetOpenTelemetryStatus ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  OtelStatus ->
  m EncJSON
runSetOpenTelemetryStatus otelStatus = do
  withNewInconsistentObjsCheck
    $ buildSchemaCacheFor (MOOpenTelemetry OtelSubobjectAll)
    $ MetadataModifier
    $ metaOpenTelemetryConfig
    . ocStatus
    .~ otelStatus
  pure successMsg

-- | Smart constructor for 'OtelExporterInfo'.
--
-- Returns a @Left qErr@ to signal a validation error. Returns @Right Nothing@
-- to signal that the exporter should be disabled without raising an error.
--
-- If this is called we assume 'OtelEnabled'
parseOtelExporterConfig ::
  Environment ->
  Set.Set OtelDataType ->
  OtelExporterConfig ->
  Either QErr OtelExporterInfo
parseOtelExporterConfig env enabledDataTypes OtelExporterConfig {..} = do
  -- First validate everything but the trace endpoint
  headers <- makeHeadersFromConf env _oecHeaders
  let mkExportReq rawEndpoint = do
        uri <-
          maybeToEither (err400 InvalidParams "Invalid URL")
            $ parseURI
            $ Text.unpack rawEndpoint
        uriRequest <-
          first (err400 InvalidParams . tshow) $ requestFromURI uri
        pure
          $ Just
          $ uriRequest
            { requestHeaders = headers ++ requestHeaders uriRequest
            }
  -- Allow telemetry endpoints to be unset when not enabled
  _oteleiTracesBaseRequest <- case _oecTracesEndpoint of
    Nothing
      | OtelTraces `Set.member` enabledDataTypes ->
          Left (err400 InvalidParams "Traces export is enabled but tracing endpoint missing")
      | otherwise -> pure Nothing -- disabled
    Just rawTracesEndpoint ->
      mkExportReq rawTracesEndpoint
  _oteleiMetricsBaseRequest <- case _oecMetricsEndpoint of
    Nothing
      | OtelMetrics `Set.member` enabledDataTypes ->
          Left (err400 InvalidParams "Metrics export is enabled but metrics endpoint missing")
      | otherwise -> pure Nothing -- disabled
    Just rawTracesEndpoint ->
      mkExportReq rawTracesEndpoint
  pure
    $ OtelExporterInfo
      { _oteleiMetricsBaseRequest,
        _oteleiTracesBaseRequest,
        _oteleiResourceAttributes =
          Map.fromList
            $ map
              (\NameValue {nv_name, nv_value} -> (nv_name, nv_value))
              _oecResourceAttributes
      }

-- Smart constructor. Consistent with defaults.
parseOtelBatchSpanProcessorConfig ::
  OtelBatchSpanProcessorConfig -> Either QErr OtelBatchSpanProcessorInfo
parseOtelBatchSpanProcessorConfig OtelBatchSpanProcessorConfig {..} = do
  _obspiMaxExportBatchSize <-
    if _obspcMaxExportBatchSize > 0
      then Right _obspcMaxExportBatchSize
      else Left (err400 InvalidParams "max_export_batch_size must be a positive integer")
  let _obspiMaxQueueSize = 4 * _obspiMaxExportBatchSize -- consistent with default value of 2048
  pure OtelBatchSpanProcessorInfo {..}
