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
-- Allows the trace endpoint to be unset if the entire OpenTelemetry system is
-- disabled.
parseOtelExporterConfig ::
  OtelStatus ->
  Environment ->
  OtelExporterConfig ->
  Either QErr (Maybe OtelExporterInfo)
parseOtelExporterConfig otelStatus env OtelExporterConfig {..} = do
  -- First validate everything but the trace endpoint
  headers <- makeHeadersFromConf env _oecHeaders
  -- Allow the trace endpoint to be unset when OpenTelemetry is disabled
  case _oecTracesEndpoint of
    Nothing ->
      case otelStatus of
        OtelDisabled ->
          pure Nothing
        OtelEnabled -> Left (err400 InvalidParams "Missing traces endpoint")
    Just rawTracesEndpoint -> do
      tracesUri <-
        maybeToEither (err400 InvalidParams "Invalid URL")
          $ parseURI
          $ Text.unpack rawTracesEndpoint
      uriRequest <-
        first (err400 InvalidParams . tshow) $ requestFromURI tracesUri
      pure
        $ Just
        $ OtelExporterInfo
          { _oteleiTracesBaseRequest =
              uriRequest
                { requestHeaders = headers ++ requestHeaders uriRequest
                },
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
