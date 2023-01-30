module Hasura.RQL.DDL.OpenTelemetry
  ( runSetOpenTelemetryConfig,
    runSetOpenTelemetryStatus,
  )
where

import Control.Lens ((.~))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Metadata.Class ()
import Hasura.Prelude
import Hasura.RQL.Types.Common (successMsg)
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.OpenTelemetry
import Hasura.RQL.Types.SchemaCache.Build

-- | Set the OpenTelemetry configuration to the provided value.
runSetOpenTelemetryConfig ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  OpenTelemetryConfig ->
  m EncJSON
runSetOpenTelemetryConfig otelConfig = do
  withNewInconsistentObjsCheck $
    buildSchemaCacheFor (MOOpenTelemetry OtelSubobjectAll) $
      MetadataModifier $
        metaOpenTelemetryConfig .~ otelConfig
  pure successMsg

-- | Set just the "status" field of the OpenTelemetry configuration.
runSetOpenTelemetryStatus ::
  (MonadError QErr m, MetadataM m, CacheRWM m) =>
  OtelStatus ->
  m EncJSON
runSetOpenTelemetryStatus otelStatus = do
  withNewInconsistentObjsCheck $
    buildSchemaCacheFor (MOOpenTelemetry OtelSubobjectAll) $
      MetadataModifier $
        metaOpenTelemetryConfig . ocStatus .~ otelStatus
  pure successMsg
