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
    scTemplate,
    scTimeoutMicroseconds,
    scEnvironment,
    DataConnectorName,
    unDataConnectorName,
    mkDataConnectorName,
    DataConnectorOptions (..),
    DataConnectorInfo (..),
    TableName (..),
    ConstraintName (..),
    ColumnName (..),
    FunctionName (..),
    CountAggregate (..),
    Literal (..),
    OrderDirection (..),
    API.GraphQLType (..),
    ScalarType (..),
    mkScalarType,
    fromGQLType,
    ExtraTableMetadata (..),
    ExtraColumnMetadata (..),
  )
where

import Autodocodec (HasCodec (codec), optionalField', requiredField', requiredFieldWith')
import Autodocodec qualified as AC
import Autodocodec.Extended (baseUrlCodec)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, genericParseJSON, genericToJSON)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J
import Data.Aeson.Types (parseEither, toJSONKeyText)
import Data.Data (Typeable)
import Data.Environment (Environment)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Extended (ToTxt (..))
import Data.Text.NonEmpty (NonEmptyText, mkNonEmptyTextUnsafe)
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue (ToErrorValue (..))
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

instance HasCodec ConnSourceConfig where
  codec = AC.bimapCodec dec enc $ AC.possiblyJointEitherCodec withValueProp inlineConfig
    where
      withValueProp =
        AC.object "DataConnectorConnSourceConfig" $
          ConnSourceConfig
            <$> requiredField' "value" AC..= value
            <*> optionalField' "template" AC..= template
            <*> optionalField' "timeout" AC..= timeout
      inlineConfig = codec @API.Config

      dec (Left config) = Right config
      dec (Right config@(API.Config jsonObj)) =
        parseEither (\o -> ConnSourceConfig config Nothing <$> (o J..:? "timeout")) jsonObj

      enc = Left

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

instance HasCodec SourceTimeout where
  codec =
    AC.dimapCodec dec enc $
      AC.disjointEitherCodec secondsCodec $
        AC.disjointEitherCodec millisecondsCodec microsecondsCodec
    where
      secondsCodec = AC.object "DataConnectorSourceTimeoutSeconds" $ requiredFieldWith' "seconds" AC.scientificCodec
      millisecondsCodec = AC.object "DataConnectorSourceTimeoutMilliseconds" $ requiredFieldWith' "milliseconds" AC.scientificCodec
      microsecondsCodec = AC.object "DataConnectorSourceTimeoutMicroseconds" $ requiredFieldWith' "microseconds" AC.scientificCodec

      dec (Left n) = SourceTimeoutSeconds $ round n
      dec (Right (Left n)) = SourceTimeoutMilliseconds $ round n
      dec (Right (Right n)) = SourceTimeoutMicroseconds $ round n

      enc (SourceTimeoutSeconds n) = Left $ fromIntegral n
      enc (SourceTimeoutMilliseconds n) = Right $ Left $ fromIntegral n
      enc (SourceTimeoutMicroseconds n) = Right $ Right $ fromIntegral n

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
    _scTemplate :: Maybe Text, -- TODO: Use Parsed Kriti Template, specify template language
    _scCapabilities :: API.Capabilities,
    _scManager :: HTTP.Manager,
    _scTimeoutMicroseconds :: Maybe Int,
    _scDataConnectorName :: DataConnectorName,
    _scEnvironment :: Environment
  }

instance Eq SourceConfig where
  SourceConfig ep1 capabilities1 config1 template1 _ timeout1 dcName1 env1 == SourceConfig ep2 capabilities2 config2 template2 _ timeout2 dcName2 env2 =
    ep1 == ep2
      && capabilities1 == capabilities2
      && config1 == config2
      && template1 == template2
      && timeout1 == timeout2
      && dcName1 == dcName2
      && env1 == env2

instance Show SourceConfig where
  show _ = "SourceConfig"

instance J.ToJSON SourceConfig where
  toJSON _ = J.String "SourceConfig"

--------------------------------------------------------------------------------

-- | Note: Currently you should not use underscores in this name.
--         This should be enforced in instances, and the `mkDataConnectorName`
--         smart constructor is available to assist.
newtype DataConnectorName = DataConnectorName {unDataConnectorName :: GQL.Name}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (ToJSON, FromJSONKey, ToJSONKey, Hashable, ToTxt)
  deriving anyclass (NFData)

instance FromJSON DataConnectorName where
  parseJSON v = (`onLeft` fail) =<< (mkDataConnectorName <$> J.parseJSON v)

mkDataConnectorName :: GQL.Name -> Either String DataConnectorName
mkDataConnectorName n =
  if ('_' `Text.elem` GQL.unName n)
    then -- Could return other errors in future.
      Left "DataConnectorName may not contain underscores."
    else Right (DataConnectorName n)

instance Witch.From DataConnectorName NonEmptyText where
  from = mkNonEmptyTextUnsafe . GQL.unName . unDataConnectorName -- mkNonEmptyTextUnsafe is safe here since GQL.Name is never empty

instance Witch.From DataConnectorName Text where
  from = GQL.unName . unDataConnectorName

data DataConnectorOptions = DataConnectorOptions
  { _dcoUri :: BaseUrl,
    _dcoDisplayName :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance HasCodec DataConnectorOptions where
  codec =
    AC.object "DataConnectorOptions" $
      DataConnectorOptions
        <$> requiredFieldWith' "uri" baseUrlCodec AC..= _dcoUri
        <*> optionalField' "display_name" AC..= _dcoDisplayName

instance FromJSON DataConnectorOptions where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON DataConnectorOptions where
  toJSON = genericToJSON hasuraJSON {J.omitNothingFields = True}

--------------------------------------------------------------------------------

data DataConnectorInfo = DataConnectorInfo
  { _dciOptions :: DataConnectorOptions,
    _dciCapabilities :: API.Capabilities,
    _dciConfigSchemaResponse :: API.ConfigSchemaResponse,
    _dciDisplayName :: Maybe Text,
    _dciReleaseName :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON DataConnectorInfo where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON DataConnectorInfo where
  toJSON = genericToJSON hasuraJSON {J.omitNothingFields = True}

--------------------------------------------------------------------------------

-- | The fully qualified name of a table. The last element in the list is the table name
-- and all other elements represent namespacing of the table name.
-- For example, for a database that has schemas, the name would be '[<schema>,<table name>]'
newtype TableName = TableName {unTableName :: NonEmpty Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (Hashable, NFData, ToJSON)

instance HasCodec TableName where
  codec = AC.dimapCodec TableName unTableName codec

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
  deriving newtype (NFData, Hashable, FromJSON, ToJSON)

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
  deriving newtype (NFData, Hashable, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

instance HasCodec ColumnName where
  codec = AC.dimapCodec ColumnName unColumnName codec

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
  deriving newtype (FromJSON, Hashable, NFData, ToJSON)

instance HasCodec FunctionName where
  codec = AC.dimapCodec FunctionName unFunctionName codec

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
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

data Literal
  = ValueLiteral ScalarType J.Value
  | ArrayLiteral ScalarType [J.Value]
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

data OrderDirection
  = Ascending
  | Descending
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)

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
  = ScalarType Text (Maybe API.GraphQLType)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, FromJSONKey, Hashable, NFData, ToJSON, ToJSONKey)

instance HasCodec ScalarType where
  codec = AC.named "ScalarType" placeholderCodecViaJSON

instance ToTxt ScalarType where
  toTxt (ScalarType name _) = name

instance ToErrorValue ScalarType where
  toErrorValue = ErrorValue.squote . toTxt

instance Witch.From ScalarType API.ScalarType where
  from (ScalarType name _) = API.ScalarType name

mkScalarType :: API.Capabilities -> API.ScalarType -> ScalarType
mkScalarType API.Capabilities {..} apiType@(API.ScalarType name) =
  ScalarType name graphQLType
  where
    graphQLType = HashMap.lookup apiType (API.unScalarTypesCapabilities _cScalarTypes) >>= API._stcGraphQLType

fromGQLType :: GQL.Name -> API.ScalarType
fromGQLType typeName =
  API.ScalarType $ GQL.unName typeName

--------------------------------------------------------------------------------

-- | This type captures backend-specific "extra" information about tables
-- and is used on types like 'DBTableMetadata'
data ExtraTableMetadata = ExtraTableMetadata
  {_etmExtraColumnMetadata :: HashMap ColumnName ExtraColumnMetadata}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data ExtraColumnMetadata = ExtraColumnMetadata
  {_ecmValueGenerated :: Maybe API.ColumnValueGenerationStrategy}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

$(makeLenses ''SourceConfig)
