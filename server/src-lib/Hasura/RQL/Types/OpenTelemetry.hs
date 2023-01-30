{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.OpenTelemetry
  ( -- * User-facing configuration (metadata)
    OpenTelemetryConfig (..),
    ocStatus,
    ocEnabledDataTypes,
    ocExporterOtlp,
    ocBatchSpanProcessor,
    emptyOpenTelemetryConfig,
    OpenTelemetryConfigSubobject (..),
    OtelStatus (..),
    OtelDataType (..),
    OtelExporterConfig (..),
    defaultOtelExporterConfig,
    OtlpProtocol (..),
    OtelBatchSpanProcessorConfig (..),
    defaultOtelBatchSpanProcessorConfig,

    -- * Parsed configuration (schema cache)
    OpenTelemetryInfo (..),
    otiExporterOtlp,
    otiBatchSpanProcessor,
    emptyOpenTelemetryInfo,
    OtelExporterInfo,
    parseOtelExporterConfig,
    getOtelExporterTracesBaseRequest,
    getOtelExporterResourceAttributes,
    OtelBatchSpanProcessorInfo,
    parseOtelBatchSpanProcessorConfig,
    getMaxExportBatchSize,
    getMaxQueueSize,
    defaultOtelBatchSpanProcessorInfo,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Environment (Environment)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics
import Hasura.Base.Error (Code (InvalidParams), QErr, err400)
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Headers
import Language.Haskell.TH.Syntax (Lift)
import Network.HTTP.Client (Request (requestHeaders), requestFromURI)
import Network.URI (parseURI)

--------------------------------------------------------------------------------

-- * User-facing configuration (metadata)

-- | Metadata configuration for all OpenTelemetry-related features
data OpenTelemetryConfig = OpenTelemetryConfig
  { _ocStatus :: OtelStatus,
    _ocEnabledDataTypes :: Set OtelDataType,
    _ocExporterOtlp :: OtelExporterConfig,
    _ocBatchSpanProcessor :: OtelBatchSpanProcessorConfig
  }
  deriving stock (Eq, Show)

instance FromJSON OpenTelemetryConfig where
  parseJSON = Aeson.withObject "OpenTelemetryConfig" $ \o ->
    OpenTelemetryConfig
      <$> o .:? "status" .!= defaultOtelStatus
      <*> o .:? "data_types" .!= defaultOtelEnabledDataTypes
      <*> o .:? "exporter_otlp" .!= defaultOtelExporterConfig
      <*> o .:? "batch_span_processor" .!= defaultOtelBatchSpanProcessorConfig

-- No `ToJSON` instance: use `openTelemetryConfigToOrdJSON` from
-- Hasura.RQL.Types.Metadata.Serialization

emptyOpenTelemetryConfig :: OpenTelemetryConfig
emptyOpenTelemetryConfig =
  OpenTelemetryConfig
    { _ocStatus = defaultOtelStatus,
      _ocEnabledDataTypes = defaultOtelEnabledDataTypes,
      _ocExporterOtlp = defaultOtelExporterConfig,
      _ocBatchSpanProcessor = defaultOtelBatchSpanProcessorConfig
    }

-- | Subsets of the fields of 'OpenTelemetryConfig', serving as metadata object
-- names for 'MetadataObjId'.
data OpenTelemetryConfigSubobject
  = -- | The entire OpenTelemetry configuration
    OtelSubobjectAll
  | OtelSubobjectExporterOtlp
  | OtelSubobjectBatchSpanProcessor
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)

-- | Should the OpenTelemetry exporter be enabled?
data OtelStatus = OtelEnabled | OtelDisabled
  deriving stock (Eq, Show)

defaultOtelStatus :: OtelStatus
defaultOtelStatus = OtelDisabled

instance FromJSON OtelStatus where
  parseJSON = \case
    Aeson.String s
      | s == "enabled" -> pure OtelEnabled
      | s == "disabled" -> pure OtelDisabled
    _ -> fail "OpenTelemetry status must be either \"enabled\" or \"disabled\""

instance ToJSON OtelStatus where
  toJSON status =
    Aeson.String $
      case status of
        OtelEnabled -> "enabled"
        OtelDisabled -> "disabled"

-- We currently only support traces
data OtelDataType
  = OtelTraces
  deriving stock (Eq, Ord, Show)

instance FromJSON OtelDataType where
  parseJSON = Aeson.withText "OtelDataType" \case
    "traces" -> pure OtelTraces
    x -> fail $ "unexpected string '" <> show x <> "'."

instance ToJSON OtelDataType where
  toJSON = \case
    OtelTraces -> Aeson.String "traces"

defaultOtelEnabledDataTypes :: Set OtelDataType
defaultOtelEnabledDataTypes = Set.empty

-- | https://opentelemetry.io/docs/reference/specification/protocol/exporter/
data OtelExporterConfig = OtelExporterConfig
  { -- | Target URL to which the exporter is going to send traces. No default.
    _oecTracesEndpoint :: Maybe Text,
    -- | The transport protocol
    _oecProtocol :: OtlpProtocol,
    -- | Key-value pairs to be used as headers to send with an export request.
    _oecHeaders :: [HeaderConf],
    -- | Attributes to send as the resource attributes of an export request. We
    -- currently only support string-valued attributes.
    _oecResourceAttributes :: [NameValue]
  }
  deriving stock (Eq, Show)

instance FromJSON OtelExporterConfig where
  parseJSON = Aeson.withObject "OtelExporterConfig" $ \o -> do
    _oecTracesEndpoint <-
      o .:? "otlp_traces_endpoint" .!= defaultOtelExporterTracesEndpoint
    _oecProtocol <-
      o .:? "protocol" .!= defaultOtelExporterProtocol
    _oecHeaders <-
      o .:? "headers" .!= defaultOtelExporterHeaders
    _oecResourceAttributes <-
      o .:? "resource_attributes" .!= defaultOtelExporterResourceAttributes
    pure OtelExporterConfig {..}

instance ToJSON OtelExporterConfig where
  toJSON (OtelExporterConfig otlpTracesEndpoint protocol headers resourceAttributes) =
    Aeson.object $
      catMaybes
        [ ("otlp_traces_endpoint" .=) <$> otlpTracesEndpoint,
          Just $ "protocol" .= protocol,
          Just $ "headers" .= headers,
          Just $ "resource_attributes" .= resourceAttributes
        ]

defaultOtelExporterConfig :: OtelExporterConfig
defaultOtelExporterConfig =
  OtelExporterConfig
    { _oecTracesEndpoint = defaultOtelExporterTracesEndpoint,
      _oecProtocol = defaultOtelExporterProtocol,
      _oecHeaders = defaultOtelExporterHeaders,
      _oecResourceAttributes = defaultOtelExporterResourceAttributes
    }

-- | Possible protocol to use with OTLP. Currently, only http/protobuf is
-- supported.
data OtlpProtocol
  = OtlpProtocolHttpProtobuf
  -- OtlpProtocolHttpJson
  -- OtlpProtocolGrpc
  deriving stock (Eq, Show)

instance FromJSON OtlpProtocol where
  parseJSON = Aeson.withText "OtlpProtocol" \case
    "http/protobuf" -> pure OtlpProtocolHttpProtobuf
    "http/json" -> fail "http/json is not supported"
    "grpc" -> fail "gRPC is not supported"
    x -> fail $ "unexpected string '" <> show x <> "'."

instance ToJSON OtlpProtocol where
  toJSON = \case
    OtlpProtocolHttpProtobuf -> Aeson.String "http/protobuf"

-- Internal helper type for JSON lists of key-value pairs
data NameValue = NameValue
  { nv_name :: Text,
    nv_value :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON NameValue where
  toJSON (NameValue {nv_name, nv_value}) =
    Aeson.object ["name" .= nv_name, "value" .= nv_value]

instance FromJSON NameValue where
  parseJSON = Aeson.withObject "name-value pair" $ \o -> do
    nv_name <- o .: "name"
    nv_value <- o .: "value"
    pure NameValue {..}

defaultOtelExporterTracesEndpoint :: Maybe Text
defaultOtelExporterTracesEndpoint = Nothing

defaultOtelExporterProtocol :: OtlpProtocol
defaultOtelExporterProtocol = OtlpProtocolHttpProtobuf

defaultOtelExporterHeaders :: [HeaderConf]
defaultOtelExporterHeaders = []

defaultOtelExporterResourceAttributes :: [NameValue]
defaultOtelExporterResourceAttributes = []

-- https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/sdk.md#batching-processor
newtype OtelBatchSpanProcessorConfig = OtelBatchSpanProcessorConfig
  { -- | The maximum batch size of every export. It must be smaller or equal to
    -- maxQueueSize (not yet configurable). Default 512.
    _obspcMaxExportBatchSize :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON OtelBatchSpanProcessorConfig where
  parseJSON = Aeson.withObject "OtelBatchSpanProcessorConfig" $ \o ->
    OtelBatchSpanProcessorConfig
      <$> o .:? "max_export_batch_size" .!= defaultMaxExportBatchSize

instance ToJSON OtelBatchSpanProcessorConfig where
  toJSON (OtelBatchSpanProcessorConfig maxExportBatchSize) =
    Aeson.object
      [ "max_export_batch_size" .= maxExportBatchSize
      ]

defaultOtelBatchSpanProcessorConfig :: OtelBatchSpanProcessorConfig
defaultOtelBatchSpanProcessorConfig =
  OtelBatchSpanProcessorConfig
    { _obspcMaxExportBatchSize = defaultMaxExportBatchSize
    }

defaultMaxExportBatchSize :: Int
defaultMaxExportBatchSize = 512

$(makeLenses ''OpenTelemetryConfig)

--------------------------------------------------------------------------------

-- * Parsed configuration (schema cache)

-- | Schema cache configuration for all OpenTelemetry-related features
data OpenTelemetryInfo = OpenTelemetryInfo
  { _otiExporterOtlp :: Maybe OtelExporterInfo,
    -- | A value of 'Nothing' indicates that the export of trace data is
    -- disabled.
    _otiBatchSpanProcessor :: Maybe OtelBatchSpanProcessorInfo
  }

emptyOpenTelemetryInfo :: OpenTelemetryInfo
emptyOpenTelemetryInfo =
  OpenTelemetryInfo
    { _otiExporterOtlp = Nothing,
      _otiBatchSpanProcessor = Nothing
    }

data OtelExporterInfo = OtelExporterInfo
  { -- | HTTP 'Request' containing (1) the target URL to which the exporter is
    -- going to send spans, and (2) the user-specified request headers.
    _oteleiTracesBaseRequest :: Request,
    -- | Attributes to send as the resource attributes of an export request. We
    -- currently only support string-valued attributes.
    _oteleiResourceAttributes :: [(Text, Text)]
  }

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
        maybeToEither (err400 InvalidParams "Invalid URL") $
          parseURI $
            Text.unpack rawTracesEndpoint
      uriRequest <-
        first (err400 InvalidParams . tshow) $ requestFromURI tracesUri
      pure $
        Just $
          OtelExporterInfo
            { _oteleiTracesBaseRequest =
                uriRequest
                  { requestHeaders = headers ++ requestHeaders uriRequest
                  },
              _oteleiResourceAttributes =
                map
                  (\NameValue {nv_name, nv_value} -> (nv_name, nv_value))
                  _oecResourceAttributes
            }

getOtelExporterTracesBaseRequest :: OtelExporterInfo -> Request
getOtelExporterTracesBaseRequest = _oteleiTracesBaseRequest

getOtelExporterResourceAttributes :: OtelExporterInfo -> [(Text, Text)]
getOtelExporterResourceAttributes = _oteleiResourceAttributes

data OtelBatchSpanProcessorInfo = OtelBatchSpanProcessorInfo
  { -- | The maximum batch size of every export. It must be smaller or equal to
    -- maxQueueSize. Default 512.
    _obspiMaxExportBatchSize :: Int,
    -- | The maximum span queue size. After the size is reached spans are
    -- dropped. Default 2048.
    _obspiMaxQueueSize :: Int
  }
  deriving (Lift)

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

getMaxExportBatchSize :: OtelBatchSpanProcessorInfo -> Int
getMaxExportBatchSize = _obspiMaxExportBatchSize

getMaxQueueSize :: OtelBatchSpanProcessorInfo -> Int
getMaxQueueSize = _obspiMaxQueueSize

-- | Defaults taken from
-- https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/sdk.md#batching-processor
defaultOtelBatchSpanProcessorInfo :: OtelBatchSpanProcessorInfo
defaultOtelBatchSpanProcessorInfo =
  OtelBatchSpanProcessorInfo
    { _obspiMaxExportBatchSize = 512,
      _obspiMaxQueueSize = 2048
    }

$(makeLenses ''OpenTelemetryInfo)
