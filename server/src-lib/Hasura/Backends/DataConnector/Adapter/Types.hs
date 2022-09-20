{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.Adapter.Types
  ( ConnSourceConfig (..),
    SourceTimeout (),
    sourceTimeoutMicroseconds,
    SourceConfig (..),
    scCapabilities,
    scConfig,
    scDataConnectorName,
    scEndpoint,
    scManager,
    scSchema,
    scTemplate,
    scTimeoutMicroseconds,
    DataConnectorName (..),
    DataConnectorOptions (..),
    DataConnectorInfo (..),
    TableName (..),
    ConstraintName (..),
    ColumnName (..),
    FunctionName (..),
    CountAggregate (..),
    Literal (..),
    OrderDirection (..),
    ScalarType (..),
    fromGQLType,
  )
where

import Autodocodec
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, genericParseJSON, genericToJSON)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J
import Data.Aeson.Types (toJSONKeyText)
import Data.Data (Typeable)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Extended (ToTxt (..))
import Data.Text.NonEmpty (NonEmptyText)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue (ToErrorValue (..))
import Hasura.GraphQL.Parser.Name.TypeSystem (_Boolean, _Float, _Int, _String)
import Hasura.Incremental (Cacheable (..))
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Servant.Client (BaseUrl)
import Witch qualified

--------------------------------------------------------------------------------

data ConnSourceConfig = ConnSourceConfig
  { -- | An arbitrary JSON payload to be passed to the agent in a
    -- header. HGE validates this against the OpenAPI Spec provided by
    -- the agent.
    value :: API.Config,
    -- | Kriti Template for transforming the supplied 'API.Config' value.
    template :: Maybe Text,
    -- | Timeout setting for HTTP requests to the agent. -- TODO: verify with lyndon
    timeout :: Maybe SourceTimeout
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData, ToJSON)

-- Default to the old style of ConnSourceConfig if a "value" field isn't present.
-- This will prevent existing configurations from breaking.
-- NOTE: This is planned to be deprecated in future once tooling is migrated.
instance FromJSON ConnSourceConfig where
  parseJSON = J.withObject "ConnSourceConfig" \o ->
    case J.lookup "value" o of
      Just _ -> ConnSourceConfig <$> o J..: "value" <*> o J..:? "template" <*> (o J..:? "timeout")
      Nothing -> ConnSourceConfig (API.Config o) Nothing <$> (o J..:? "timeout")

-- TODO: Write a proper codec, and use it to derive FromJSON and ToJSON
-- instances.
instance HasCodec ConnSourceConfig where
  codec = named "DataConnectorConnConfiguration" $ placeholderCodecViaJSON

instance Cacheable ConnSourceConfig where
  unchanged _ = (==)

--------------------------------------------------------------------------------

-- NOTE: There may be a time type with units datatype already available somewhere
data SourceTimeout
  = SourceTimeoutSeconds Int
  | SourceTimeoutMilliseconds Int
  | SourceTimeoutMicroseconds Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

sourceTimeoutMicroseconds :: SourceTimeout -> Int
sourceTimeoutMicroseconds = \case
  SourceTimeoutSeconds s -> s * 1000000
  SourceTimeoutMilliseconds m -> m * 1000
  SourceTimeoutMicroseconds u -> u

instance FromJSON SourceTimeout where
  parseJSON = J.withObject "SourceTimeout" \o ->
    case J.toList o of
      [("seconds", n)] -> convertTimeout n "seconds" SourceTimeoutSeconds
      [("milliseconds", n)] -> convertTimeout n "milliseconds" SourceTimeoutMilliseconds
      [("microseconds", n)] -> convertTimeout n "microseconds" SourceTimeoutMicroseconds
      _ -> fail "Invalid SourceTimeout. Formats include: {seconds: Int}, {milliseconds: Int}, {microseconds: Int}"
    where
      convertTimeout n l m = J.withScientific l (\s -> pure $ m (round s)) n

instance ToJSON SourceTimeout where
  toJSON (SourceTimeoutSeconds t) = J.object ["seconds" J..= t]
  toJSON (SourceTimeoutMilliseconds t) = J.object ["milliseconds" J..= t]
  toJSON (SourceTimeoutMicroseconds t) = J.object ["microseconds" J..= t]

--------------------------------------------------------------------------------

data SourceConfig = SourceConfig
  { _scEndpoint :: BaseUrl,
    _scConfig :: API.Config,
    _scTemplate :: Maybe Text, -- TODO: Use Parsed Kriti Template
    _scCapabilities :: API.Capabilities,
    _scSchema :: API.SchemaResponse,
    _scManager :: HTTP.Manager,
    _scTimeoutMicroseconds :: Maybe Int,
    _scDataConnectorName :: DataConnectorName
  }

instance Eq SourceConfig where
  SourceConfig ep1 capabilities1 config1 template1 schema1 _ timeout1 dcName1 == SourceConfig ep2 capabilities2 config2 template2 schema2 _ timeout2 dcName2 =
    ep1 == ep2
      && capabilities1 == capabilities2
      && config1 == config2
      && template1 == template2
      && schema1 == schema2
      && timeout1 == timeout2
      && dcName1 == dcName2

instance Show SourceConfig where
  show _ = "SourceConfig"

instance J.ToJSON SourceConfig where
  toJSON _ = J.String "SourceConfig"

instance Cacheable SourceConfig where
  unchanged _ = (==)

--------------------------------------------------------------------------------

newtype DataConnectorName = DataConnectorName {unDataConnectorName :: NonEmptyText}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable, ToTxt)
  deriving anyclass (Cacheable, NFData)

instance Witch.From DataConnectorName NonEmptyText

data DataConnectorOptions = DataConnectorOptions
  {_dcoUri :: BaseUrl}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Cacheable)

instance FromJSON DataConnectorOptions where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON DataConnectorOptions where
  toJSON = genericToJSON hasuraJSON

--------------------------------------------------------------------------------

data DataConnectorInfo = DataConnectorInfo
  { _dciOptions :: DataConnectorOptions,
    _dciCapabilities :: API.Capabilities,
    _dciConfigSchemaResponse :: API.ConfigSchemaResponse
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON DataConnectorInfo where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON DataConnectorInfo where
  toJSON = genericToJSON hasuraJSON

instance Cacheable DataConnectorInfo where
  unchanged a dci0 dci1 =
    unchanged a (_dciOptions dci0) (_dciOptions dci1)
      && _dciCapabilities dci0 == _dciCapabilities dci1
      && _dciConfigSchemaResponse dci0 == _dciConfigSchemaResponse dci1

--------------------------------------------------------------------------------

-- | The fully qualified name of a table. The last element in the list is the table name
-- and all other elements represent namespacing of the table name.
-- For example, for a database that has schemas, the name would be '[<schema>,<table name>]'
newtype TableName = TableName {unTableName :: NonEmpty Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (Cacheable, Hashable, NFData, ToJSON)

instance FromJSON TableName where
  parseJSON value =
    TableName <$> J.parseJSON value
      -- Fallback parsing of a single string to support older metadata
      <|> J.withText "TableName" (\text -> pure . TableName $ text :| []) value

instance ToJSONKey TableName where
  toJSONKey = toJSONKeyText toTxt

instance Witch.From API.TableName TableName where
  from (API.TableName n) = TableName n

instance Witch.From TableName API.TableName where
  from (TableName n) = API.TableName n

instance ToTxt TableName where
  toTxt = Text.intercalate "." . NonEmpty.toList . unTableName

instance ToErrorValue TableName where
  toErrorValue = ErrorValue.squote . toTxt

--------------------------------------------------------------------------------

newtype ConstraintName = ConstraintName {unConstraintName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (NFData, Hashable, Cacheable, FromJSON, ToJSON)

instance Witch.From API.ConstraintName ConstraintName where
  from (API.ConstraintName n) = ConstraintName n

instance Witch.From ConstraintName API.ConstraintName where
  from (ConstraintName n) = API.ConstraintName n

instance ToTxt ConstraintName where
  toTxt = unConstraintName

instance ToErrorValue ConstraintName where
  toErrorValue = ErrorValue.squote . unConstraintName

--------------------------------------------------------------------------------

newtype ColumnName = ColumnName {unColumnName :: Text}
  deriving stock (Eq, Ord, Show, Generic, Data)
  deriving newtype (NFData, Hashable, Cacheable, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

instance Witch.From API.ColumnName ColumnName where
  from (API.ColumnName n) = ColumnName n

instance Witch.From ColumnName API.ColumnName where
  from (ColumnName n) = API.ColumnName n

instance ToTxt ColumnName where
  toTxt = unColumnName

instance ToErrorValue ColumnName where
  toErrorValue = ErrorValue.squote . unColumnName

--------------------------------------------------------------------------------

newtype FunctionName = FunctionName {unFunctionName :: NonEmpty Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (Cacheable, FromJSON, Hashable, NFData, ToJSON)

instance ToJSONKey FunctionName where
  toJSONKey = toJSONKeyText toTxt

instance ToTxt FunctionName where
  toTxt = Text.intercalate "." . NonEmpty.toList . unFunctionName

instance ToErrorValue FunctionName where
  toErrorValue = ErrorValue.squote . toTxt

--------------------------------------------------------------------------------

data CountAggregate
  = StarCount
  | ColumnCount ColumnName
  | ColumnDistinctCount ColumnName
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

data Literal
  = ValueLiteral J.Value
  | ArrayLiteral [J.Value]
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (Cacheable, Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

data OrderDirection
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance ToJSON OrderDirection where
  toJSON = J.genericToJSON J.defaultOptions

instance Witch.From API.OrderDirection OrderDirection where
  from API.Ascending = Ascending
  from API.Descending = Descending

instance Witch.From OrderDirection API.OrderDirection where
  from Ascending = API.Ascending
  from Descending = API.Descending

--------------------------------------------------------------------------------

data ScalarType
  = StringTy
  | NumberTy
  | BoolTy
  | CustomTy Text
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Cacheable, FromJSON, FromJSONKey, Hashable, NFData, ToJSON, ToJSONKey)

instance ToTxt ScalarType where
  toTxt = tshow

instance ToErrorValue ScalarType where
  toErrorValue = ErrorValue.squote . tshow

instance Witch.From API.ScalarType ScalarType where
  from = \case
    API.StringTy -> StringTy
    API.NumberTy -> NumberTy
    API.BoolTy -> BoolTy
    API.CustomTy name -> CustomTy name

instance Witch.From ScalarType API.ScalarType where
  from = \case
    StringTy -> API.StringTy
    NumberTy -> API.NumberTy
    BoolTy -> API.BoolTy
    CustomTy name -> API.CustomTy name

fromGQLType :: GQL.Name -> ScalarType
fromGQLType typeName =
  if
      | typeName == _String -> StringTy
      | typeName == _Int -> NumberTy
      | typeName == _Float -> NumberTy
      | typeName == _Boolean -> BoolTy
      | otherwise -> CustomTy $ GQL.unName typeName

--------------------------------------------------------------------------------

$(makeLenses ''SourceConfig)
