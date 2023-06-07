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
    NameValue (..),

    -- * Parsed configuration (schema cache)
    OpenTelemetryInfo (..),
    otiExporterOtlp,
    otiBatchSpanProcessor,
    emptyOpenTelemetryInfo,
    OtelExporterInfo (..),
    getOtelExporterTracesBaseRequest,
    getOtelExporterResourceAttributes,
    OtelBatchSpanProcessorInfo (..),
    getMaxExportBatchSize,
    getMaxQueueSize,
    defaultOtelBatchSpanProcessorInfo,
  )
where

import Autodocodec (HasCodec, optionalField, optionalFieldWithDefault, optionalFieldWithDefault', requiredField', (<?>))
import Autodocodec qualified as AC
import Autodocodec.Extended (boundedEnumCodec)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON (..), (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics
import Hasura.Prelude hiding (first)
import Hasura.RQL.Types.Headers (HeaderConf)
import Language.Haskell.TH.Syntax (Lift)
import Network.HTTP.Client (Request)

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

instance HasCodec OpenTelemetryConfig where
  codec =
    AC.object "OpenTelemetryConfig"
      $ OpenTelemetryConfig
      <$> optionalFieldWithDefault' "status" defaultOtelStatus
      AC..= _ocStatus
        <*> optionalFieldWithDefault' "data_types" defaultOtelEnabledDataTypes
      AC..= _ocEnabledDataTypes
        <*> optionalFieldWithDefault' "exporter_otlp" defaultOtelExporterConfig
      AC..= _ocExporterOtlp
        <*> optionalFieldWithDefault' "batch_span_processor" defaultOtelBatchSpanProcessorConfig
      AC..= _ocBatchSpanProcessor

instance FromJSON OpenTelemetryConfig where
  parseJSON = J.withObject "OpenTelemetryConfig" $ \o ->
    OpenTelemetryConfig
      <$> o
      .:? "status"
      .!= defaultOtelStatus
      <*> o
      .:? "data_types"
      .!= defaultOtelEnabledDataTypes
      <*> o
      .:? "exporter_otlp"
      .!= defaultOtelExporterConfig
      <*> o
      .:? "batch_span_processor"
      .!= defaultOtelBatchSpanProcessorConfig

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
  deriving stock (Eq, Show, Bounded, Enum)

defaultOtelStatus :: OtelStatus
defaultOtelStatus = OtelDisabled

instance HasCodec OtelStatus where
  codec = boundedEnumCodec \case
    OtelEnabled -> "enabled"
    OtelDisabled -> "disabled"

instance FromJSON OtelStatus where
  parseJSON = \case
    J.String s
      | s == "enabled" -> pure OtelEnabled
      | s == "disabled" -> pure OtelDisabled
    _ -> fail "OpenTelemetry status must be either \"enabled\" or \"disabled\""

instance ToJSON OtelStatus where
  toJSON status =
    J.String
      $ case status of
        OtelEnabled -> "enabled"
        OtelDisabled -> "disabled"

-- We currently only support traces
data OtelDataType
  = OtelTraces
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance HasCodec OtelDataType where
  codec = boundedEnumCodec \case
    OtelTraces -> "traces"

instance FromJSON OtelDataType where
  parseJSON = J.withText "OtelDataType" \case
    "traces" -> pure OtelTraces
    x -> fail $ "unexpected string '" <> show x <> "'."

instance ToJSON OtelDataType where
  toJSON = \case
    OtelTraces -> J.String "traces"

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

instance HasCodec OtelExporterConfig where
  codec =
    AC.object "OtelExporterConfig"
      $ OtelExporterConfig
      <$> optionalField "otlp_traces_endpoint" tracesEndpointDoc
      AC..= _oecTracesEndpoint
        <*> optionalFieldWithDefault "protocol" defaultOtelExporterProtocol protocolDoc
      AC..= _oecProtocol
        <*> optionalFieldWithDefault "headers" defaultOtelExporterHeaders headersDoc
      AC..= _oecHeaders
        <*> optionalFieldWithDefault "resource_attributes" defaultOtelExporterResourceAttributes attrsDoc
      AC..= _oecResourceAttributes
    where
      tracesEndpointDoc = "Target URL to which the exporter is going to send traces. No default."
      protocolDoc = "The transport protocol"
      headersDoc = "Key-value pairs to be used as headers to send with an export request."
      attrsDoc = "Attributes to send as the resource attributes of an export request. We currently only support string-valued attributes."

instance FromJSON OtelExporterConfig where
  parseJSON = J.withObject "OtelExporterConfig" $ \o -> do
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
    J.object
      $ catMaybes
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
  deriving stock (Eq, Show, Bounded, Enum)

instance HasCodec OtlpProtocol where
  codec =
    ( boundedEnumCodec \case
        OtlpProtocolHttpProtobuf -> "http/protobuf"
    )
      <?> "Possible protocol to use with OTLP. Currently, only http/protobuf is supported."

instance FromJSON OtlpProtocol where
  parseJSON = J.withText "OtlpProtocol" \case
    "http/protobuf" -> pure OtlpProtocolHttpProtobuf
    "http/json" -> fail "http/json is not supported"
    "grpc" -> fail "gRPC is not supported"
    x -> fail $ "unexpected string '" <> show x <> "'."

instance ToJSON OtlpProtocol where
  toJSON = \case
    OtlpProtocolHttpProtobuf -> J.String "http/protobuf"

-- Internal helper type for JSON lists of key-value pairs
data NameValue = NameValue
  { nv_name :: Text,
    nv_value :: Text
  }
  deriving stock (Eq, Show)

instance HasCodec NameValue where
  codec =
    AC.object
      "OtelNameValue"
      ( NameValue
          <$> requiredField' "name"
          AC..= nv_name
            <*> requiredField' "value"
          AC..= nv_value
      )
      <?> "Internal helper type for JSON lists of key-value pairs"

instance ToJSON NameValue where
  toJSON (NameValue {nv_name, nv_value}) =
    J.object ["name" .= nv_name, "value" .= nv_value]

instance FromJSON NameValue where
  parseJSON = J.withObject "name-value pair" $ \o -> do
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

instance HasCodec OtelBatchSpanProcessorConfig where
  codec =
    AC.object "OtelBatchSpanProcessorConfig"
      $ OtelBatchSpanProcessorConfig
      <$> optionalFieldWithDefault "max_export_batch_size" defaultMaxExportBatchSize maxSizeDoc
      AC..= _obspcMaxExportBatchSize
    where
      maxSizeDoc = "The maximum batch size of every export. It must be smaller or equal to maxQueueSize (not yet configurable). Default 512."

instance FromJSON OtelBatchSpanProcessorConfig where
  parseJSON = J.withObject "OtelBatchSpanProcessorConfig" $ \o ->
    OtelBatchSpanProcessorConfig
      <$> o
      .:? "max_export_batch_size"
      .!= defaultMaxExportBatchSize

instance ToJSON OtelBatchSpanProcessorConfig where
  toJSON (OtelBatchSpanProcessorConfig maxExportBatchSize) =
    J.object
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
    --
    -- Using Data.Map.Strict over Data.Hashmap.Strict because currently the
    -- only operations on data are (1) folding and (2) union with a small
    -- map of default attributes, and Map should be is faster than HashMap for
    -- the latter.
    _oteleiResourceAttributes :: Map Text Text
  }

getOtelExporterTracesBaseRequest :: OtelExporterInfo -> Request
getOtelExporterTracesBaseRequest = _oteleiTracesBaseRequest

getOtelExporterResourceAttributes :: OtelExporterInfo -> Map Text Text
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
