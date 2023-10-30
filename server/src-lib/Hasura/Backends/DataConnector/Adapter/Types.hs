{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.DataConnector.Adapter.Types
  ( ConnSourceConfig (..),
    TemplateVariableName (..),
    TemplateVariableSource (..),
    SourceTimeout (),
    sourceTimeoutMicroseconds,
    SourceConfig (..),
    scCapabilities,
    scConfig,
    scDataConnectorName,
    scEndpoint,
    scManager,
    scTemplate,
    scTemplateVariables,
    scTimeoutMicroseconds,
    scEnvironment,
    DataConnectorOptions (..),
    DataConnectorInfo (..),
    TableName (..),
    ConstraintName (..),
    ColumnName (..),
    ColumnPath (..),
    FunctionName (..),
    FunctionReturnType (..),
    CountAggregate (..),
    Literal (..),
    OrderDirection (..),
    API.GraphQLType (..),
    ScalarType (..),
    ArgumentExp (..),
    fromGQLType,
    ExtraTableMetadata (..),
    ExtraColumnMetadata (..),
    module Hasura.RQL.Types.DataConnector,
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
import Data.Aeson.Types qualified as J
import Data.Environment (Environment)
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.OpenApi (ToSchema)
import Data.Text qualified as Text
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue (ToErrorValue (..))
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp qualified as IR
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.BackendType (BackendType (..))
import Hasura.RQL.Types.DataConnector
import Language.GraphQL.Draft.Syntax qualified as GQL
import Network.HTTP.Client qualified as HTTP
import Servant.Client (BaseUrl)
import Witch qualified

--------------------------------------------------------------------------------

data ConnSourceConfig = ConnSourceConfig
  { -- | An arbitrary JSON payload to be passed to the agent in a
    -- header. HGE validates this against the OpenAPI Spec provided by
    -- the agent.
    _cscValue :: API.Config,
    -- | Kriti Template for transforming the supplied 'API.Config' value.
    _cscTemplate :: Maybe Text,
    -- | Definitions of variables that can be accessed in the Kriti template via $vars
    _cscTemplateVariables :: Maybe (HashMap TemplateVariableName TemplateVariableSource),
    -- | Timeout setting for HTTP requests to the agent. -- TODO: verify with lyndon
    _cscTimeout :: Maybe SourceTimeout
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- Default to the old style of ConnSourceConfig if a "value" field isn't present.
-- This will prevent existing configurations from breaking.
-- NOTE: This is planned to be deprecated in future once tooling is migrated.
instance FromJSON ConnSourceConfig where
  parseJSON = J.withObject "ConnSourceConfig" \o ->
    case J.lookup "value" o of
      Just _ ->
        ConnSourceConfig
          <$> o
          J..: "value"
          <*> o
          J..:? "template"
          <*> o
          J..:? "template_variables"
          <*> o
          J..:? "timeout"
      Nothing -> ConnSourceConfig (API.Config o) Nothing Nothing <$> (o J..:? "timeout")

instance ToJSON ConnSourceConfig where
  toJSON ConnSourceConfig {..} =
    J.object
      . catMaybes
      $ [ Just $ "value" J..= _cscValue,
          Just $ "template" J..= _cscTemplate,
          ("template_variables" J..=) <$> _cscTemplateVariables,
          Just $ "timeout" J..= _cscTimeout
        ]

instance HasCodec ConnSourceConfig where
  codec = AC.bimapCodec dec enc $ AC.possiblyJointEitherCodec withValueProp inlineConfig
    where
      withValueProp =
        AC.object "DataConnectorConnSourceConfig"
          $ ConnSourceConfig
          <$> requiredField' "value"
          AC..= _cscValue
            <*> optionalField' "template"
          AC..= _cscTemplate
            <*> optionalField' "template_variables"
          AC..= _cscTemplateVariables
            <*> optionalField' "timeout"
          AC..= _cscTimeout
      inlineConfig = codec @API.Config

      dec (Left config) = Right config
      dec (Right config@(API.Config jsonObj)) =
        parseEither (\o -> ConnSourceConfig config Nothing Nothing <$> (o J..:? "timeout")) jsonObj

      enc = Left

newtype TemplateVariableName = TemplateVariableName {unTemplateVariableName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
  deriving anyclass (Hashable, NFData)

instance HasCodec TemplateVariableName where
  codec =
    AC.named "TemplateVariableName"
      $ AC.dimapCodec TemplateVariableName unTemplateVariableName codec
      AC.<?> "The name of the template variable"

data TemplateVariableSource
  = TemplateVariableDynamicFromFile FilePath
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance FromJSON TemplateVariableSource where
  parseJSON = J.withObject "TemplateVariableSource" \o -> do
    (typeTxt :: Text) <- o J..: "type"
    case typeTxt of
      "dynamic_from_file" ->
        TemplateVariableDynamicFromFile <$> o J..: "filepath"
      unknownType -> fail ("Unknown type: " <> Text.unpack unknownType) J.<?> J.Key "type"

instance ToJSON TemplateVariableSource where
  toJSON = \case
    TemplateVariableDynamicFromFile filepath ->
      J.object
        [ "type" J..= ("dynamic_from_file" :: Text),
          "filepath" J..= filepath
        ]

instance HasCodec TemplateVariableSource where
  codec =
    AC.object "TemplateVariableSource"
      $ AC.discriminatedUnionCodec "type" enc dec
    where
      dynamicFromFileCodec = requiredField' "filepath"
      enc = \case
        TemplateVariableDynamicFromFile filepath -> ("dynamic_from_file", AC.mapToEncoder filepath dynamicFromFileCodec)
      dec =
        HashMap.fromList
          [ ("dynamic_from_file", ("TemplateVariableDynamicFromFile", AC.mapToDecoder TemplateVariableDynamicFromFile dynamicFromFileCodec))
          ]

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
    AC.dimapCodec dec enc
      $ AC.disjointEitherCodec secondsCodec
      $ AC.disjointEitherCodec millisecondsCodec microsecondsCodec
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
    _scTemplateVariables :: HashMap TemplateVariableName TemplateVariableSource,
    _scCapabilities :: API.Capabilities,
    _scManager :: HTTP.Manager,
    _scTimeoutMicroseconds :: Maybe Int,
    _scDataConnectorName :: DataConnectorName,
    _scEnvironment :: Environment
  }

instance Eq SourceConfig where
  SourceConfig ep1 config1 template1 templateVars1 capabilities1 _ timeout1 dcName1 env1 == SourceConfig ep2 config2 template2 templateVars2 capabilities2 _ timeout2 dcName2 env2 =
    ep1
      == ep2
      && config1
      == config2
      && template1
      == template2
      && templateVars1
      == templateVars2
      && capabilities1
      == capabilities2
      && timeout1
      == timeout2
      && dcName1
      == dcName2
      && env1
      == env2

instance Show SourceConfig where
  show _ = "SourceConfig"

instance J.ToJSON SourceConfig where
  toJSON _ = J.String "SourceConfig"

--------------------------------------------------------------------------------

-- | This represents what information can be known about the return type of a user-defined function.
--   For now, either the return type will be the name of a table that exists in the schema,
--   or "Unknown" - implying that this information can be derived from another source,
--   or if there is no other source, then it is an error.
--   In future, this type may be extended with additional constructors including scalar and row types
--   from the Logical Models feature.
--
--   Note: This is very similar to ComputedFieldReturnType defined above.
--         The two types may be unified in future.
data FunctionReturnType
  = FunctionReturnsTable TableName
  | FunctionReturnsUnknown
  deriving (Show, Eq, NFData, Hashable, Generic)
  deriving (ToSchema, ToJSON, FromJSON) via AC.Autodocodec FunctionReturnType

instance AC.HasCodec FunctionReturnType where
  codec =
    AC.named "FunctionReturnType"
      $ AC.object "FunctionReturnType"
      $ AC.discriminatedUnionCodec "type" enc dec
    where
      typeField = pure ()
      tableField = AC.requiredField' "table"
      enc = \case
        FunctionReturnsTable rt -> ("table", AC.mapToEncoder rt tableField)
        FunctionReturnsUnknown -> ("inferred", AC.mapToEncoder () typeField) -- We hook into the type field because it's madatory
      dec =
        HashMap.fromList
          [ ("table", ("TableFunctionResponse", AC.mapToDecoder FunctionReturnsTable tableField)),
            ("inferred", ("InferredFunctionResponse", AC.mapToDecoder (const FunctionReturnsUnknown) typeField))
          ]

------------

data DataConnectorOptions = DataConnectorOptions
  { _dcoUri :: BaseUrl,
    _dcoDisplayName :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Generic)

instance HasCodec DataConnectorOptions where
  codec =
    AC.object "DataConnectorOptions"
      $ DataConnectorOptions
      <$> requiredFieldWith' "uri" baseUrlCodec
      AC..= _dcoUri
        <*> optionalField' "display_name"
      AC..= _dcoDisplayName

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
    TableName
      <$> J.parseJSON value
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
  deriving newtype (NFData, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

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

instance Witch.From ColumnName ColumnPath where
  from = CPColumn

--------------------------------------------------------------------------------

data ColumnPath
  = CPPath (NonEmpty ColumnName)
  | CPColumn (ColumnName)
  deriving stock (Eq, Ord, Show, Generic)
  deriving (FromJSON, ToJSON, ToSchema) via AC.Autodocodec ColumnPath
  deriving anyclass (Hashable, NFData)

instance HasCodec ColumnPath where
  codec = AC.disjointMatchChoiceCodec pathCodec columnCodec chooser
    where
      pathCodec = AC.dimapCodec CPPath id (codec @(NonEmpty ColumnName))
      columnCodec = AC.dimapCodec CPColumn id (codec @ColumnName)
      chooser = \case
        CPPath p -> Left p
        CPColumn c -> Right c

instance ToJSONKey ColumnPath

instance FromJSONKey ColumnPath

instance Witch.From API.ColumnSelector ColumnPath where
  from = \case
    API.ColumnSelectorPath p -> CPPath $ Witch.from <$> p
    API.ColumnSelectorColumn c -> CPColumn $ Witch.from c

instance Witch.From ColumnPath API.ColumnSelector where
  from = \case
    CPPath p -> API.ColumnSelectorPath $ Witch.from <$> p
    CPColumn c -> API.ColumnSelectorColumn $ Witch.from c

--------------------------------------------------------------------------------

newtype FunctionName = FunctionName {unFunctionName :: NonEmpty Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, Hashable, NFData, ToJSON)

instance Witch.From FunctionName API.FunctionName

instance Witch.From API.FunctionName FunctionName

instance Witch.From (NonEmpty Text) FunctionName

instance HasCodec FunctionName where
  codec = AC.dimapCodec FunctionName unFunctionName codec

instance ToJSONKey FunctionName where
  toJSONKey = toJSONKeyText toTxt

instance ToTxt FunctionName where
  toTxt = Text.intercalate "." . NonEmpty.toList . unFunctionName

instance ToErrorValue FunctionName where
  toErrorValue = ErrorValue.squote . toTxt

-- Modified from Hasura.Backends.Postgres.Types.Function
-- Initially just handles literal input arguments.
data ArgumentExp a
  = -- | Table row accessor
    --   AETableRow
    -- | -- | Hardcoded reference to @hdb_catalog.hdb_action_log.response_payload@
    --   AEActionResponsePayload
    -- | -- | JSON/JSONB hasura session variable object
    --   AESession a
    -- |
    AEInput a
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (Hashable a) => Hashable (ArgumentExp a)

--------------------------------------------------------------------------------

data CountAggregate v
  = StarCount
  | ColumnCount (ColumnName, IR.AnnRedactionExp 'DataConnector v)
  | ColumnDistinctCount (ColumnName, IR.AnnRedactionExp 'DataConnector v)
  deriving (Generic)

deriving stock instance
  (Backend 'DataConnector, Show (IR.AnnRedactionExp 'DataConnector v), Show v) =>
  Show (CountAggregate v)

deriving stock instance (Backend 'DataConnector) => Functor CountAggregate

deriving stock instance (Backend 'DataConnector) => Foldable CountAggregate

deriving stock instance (Backend 'DataConnector) => Traversable CountAggregate

deriving stock instance
  (Backend 'DataConnector, Eq (IR.AnnRedactionExp 'DataConnector v), Eq v) =>
  Eq (CountAggregate v)

deriving stock instance
  (Backend 'DataConnector, Data (IR.AnnRedactionExp 'DataConnector v), Data v) =>
  Data (CountAggregate v)

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

newtype ScalarType = ScalarType {unScalarType :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable, NFData)
  deriving newtype (FromJSONKey, ToJSONKey)
  deriving (FromJSON, ToJSON) via AC.Autodocodec ScalarType

instance HasCodec ScalarType where
  codec = AC.named "ScalarType" $ AC.dimapCodec ScalarType unScalarType codec

instance ToTxt ScalarType where
  toTxt (ScalarType name) = name

instance ToErrorValue ScalarType where
  toErrorValue = ErrorValue.squote . toTxt

instance Witch.From ScalarType API.ScalarType where
  from (ScalarType name) = API.ScalarType name

instance Witch.From API.ScalarType ScalarType where
  from (API.ScalarType name) = ScalarType name

fromGQLType :: GQL.Name -> API.ScalarType
fromGQLType typeName =
  API.ScalarType $ GQL.unName typeName

--------------------------------------------------------------------------------

-- | This type captures backend-specific "extra" information about tables
-- and is used on types like 'DBTableMetadata'
data ExtraTableMetadata = ExtraTableMetadata
  { _etmTableType :: API.TableType,
    _etmExtraColumnMetadata :: HashMap ColumnName ExtraColumnMetadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

data ExtraColumnMetadata = ExtraColumnMetadata
  {_ecmValueGenerated :: Maybe API.ColumnValueGenerationStrategy}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, Hashable, NFData, ToJSON)

--------------------------------------------------------------------------------

$(makeLenses ''SourceConfig)

instance Has API.ScalarTypesCapabilities SourceConfig where
  hasLens = scCapabilities . API.cScalarTypes
