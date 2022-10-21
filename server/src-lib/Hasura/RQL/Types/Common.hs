{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Common
  ( RelName (..),
    relNameToTxt,
    fromRemoteRelationship,
    RelType (..),
    relTypeToTxt,
    OID (..),
    FieldName (..),
    Fields,
    InsertOrder (..),
    ToAesonPairs (..),
    InpValInfo (..),
    SystemDefined (..),
    isSystemDefined,
    SQLGenCtx (..),
    successMsg,
    InputWebhook (..),
    ResolvedWebhook (..),
    resolveWebhook,
    Timeout (..),
    defaultActionTimeoutSecs,
    UrlConf (..),
    resolveUrlConf,
    getEnv,
    SourceName (..),
    defaultSource,
    sourceNameToText,
    JsonAggSelect (..),
    MetricsConfig (..),
    emptyMetricsConfig,
    PGConnectionParams (..),
    getPGConnectionStringFromParams,
    getConnOptionsFromConnParams,
    Comment (..),
    commentToMaybeText,
    commentFromMaybeText,
    EnvRecord (..),
    ApolloFederationConfig (..),
    ApolloFederationVersion (..),
    isApolloFedV1enabled,
    RemoteRelationshipG (..),
    rrDefinition,
    rrName,
  )
where

import Autodocodec
  ( HasCodec (codec),
    bimapCodec,
    dimapCodec,
    disjointEitherCodec,
    optionalFieldOrNull',
    requiredField',
    stringConstCodec,
  )
import Autodocodec qualified as AC
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Bifunctor (bimap)
import Data.Environment qualified as Env
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.URL.Template
import Database.PG.Query qualified as PG
import Hasura.Base.Error
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.EncJSON
import Hasura.GraphQL.Schema.Options qualified as Options
import Hasura.Incremental (Cacheable (..))
import Hasura.Metadata.DTO.Utils (fromEnvCodec)
import Hasura.Prelude
import Hasura.RQL.DDL.Headers ()
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax qualified as TH
import Network.URI
import PostgreSQL.Binary.Decoding qualified as PD

newtype RelName = RelName {getRelTxt :: NonEmptyText}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      FromJSON,
      FromJSONKey,
      ToJSON,
      ToJSONKey,
      PG.ToPrepArg,
      PG.FromCol,
      Generic,
      NFData,
      Cacheable
    )

instance ToTxt RelName where
  toTxt = relNameToTxt

instance HasCodec RelName where
  codec = dimapCodec RelName getRelTxt codec

relNameToTxt :: RelName -> Text
relNameToTxt = unNonEmptyText . getRelTxt

fromRemoteRelationship :: RelName -> FieldName
fromRemoteRelationship = FieldName . relNameToTxt

data RelType
  = ObjRel
  | ArrRel
  deriving (Show, Eq, Ord, Generic, Data)

instance NFData RelType

instance Hashable RelType

instance Cacheable RelType

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array") = return ArrRel
  parseJSON _ = fail "expecting either 'object' or 'array' for rel_type"

instance PG.FromCol RelType where
  fromCol bs = flip PG.fromColHelper bs $
    PD.enum $ \case
      "object" -> Just ObjRel
      "array" -> Just ArrRel
      _ -> Nothing

relTypeToTxt :: RelType -> Text
relTypeToTxt ObjRel = "object"
relTypeToTxt ArrRel = "array"

data JsonAggSelect
  = JASMultipleRows
  | JASSingleObject
  deriving (Show, Eq, Generic)

instance Hashable JsonAggSelect

instance ToJSON JsonAggSelect where
  toJSON = \case
    JASMultipleRows -> "multiple_rows"
    JASSingleObject -> "single_row"

data InsertOrder = BeforeParent | AfterParent
  deriving (Show, Eq, Generic)

instance NFData InsertOrder

instance Hashable InsertOrder

instance Cacheable InsertOrder

instance HasCodec InsertOrder where
  codec =
    stringConstCodec
      [ (BeforeParent, "before_parent"),
        (AfterParent, "after_parent")
      ]

instance FromJSON InsertOrder where
  parseJSON (String t)
    | t == "before_parent" = pure BeforeParent
    | t == "after_parent" = pure AfterParent
  parseJSON _ =
    fail "insertion_order should be 'before_parent' or 'after_parent'"

instance ToJSON InsertOrder where
  toJSON = \case
    BeforeParent -> String "before_parent"
    AfterParent -> String "after_parent"

-- | Postgres OIDs. <https://www.postgresql.org/docs/12/datatype-oid.html>
newtype OID = OID {unOID :: Int}
  deriving (Show, Eq, NFData, Hashable, ToJSON, FromJSON, PG.FromCol, Cacheable)

newtype FieldName = FieldName {getFieldNameTxt :: Text}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      FromJSON,
      ToJSON,
      FromJSONKey,
      ToJSONKey,
      Data,
      Generic,
      IsString,
      NFData,
      Cacheable,
      Semigroup
    )

instance ToTxt FieldName where
  toTxt (FieldName c) = c

-- The field name here is the GraphQL alias, i.e, the name with which the field
-- should appear in the response
type Fields a = [(FieldName, a)]

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data SourceName
  = SNDefault
  | SNName NonEmptyText
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SourceName where
  parseJSON = withText "String" $ \case
    "default" -> pure SNDefault
    t -> SNName <$> parseJSON (String t)

instance HasCodec SourceName where
  codec = dimapCodec dec enc nonEmptyTextCodec
    where
      dec t
        | t == defaultSourceName = SNDefault
        | otherwise = SNName t

      enc SNDefault = defaultSourceName
      enc (SNName t) = t

sourceNameToText :: SourceName -> Text
sourceNameToText = \case
  SNDefault -> unNonEmptyText defaultSourceName
  SNName t -> unNonEmptyText t

instance ToJSON SourceName where
  toJSON = String . sourceNameToText

instance ToTxt SourceName where
  toTxt = sourceNameToText

instance ToErrorValue SourceName where
  toErrorValue = \case
    SNDefault -> "default"
    SNName t -> ErrorValue.squote (unNonEmptyText t)

instance ToJSONKey SourceName

instance Hashable SourceName

instance NFData SourceName

instance Cacheable SourceName

defaultSource :: SourceName
defaultSource = SNDefault

defaultSourceName :: NonEmptyText
defaultSourceName = mkNonEmptyTextUnsafe "default"

data InpValInfo = InpValInfo
  { _iviDesc :: Maybe G.Description,
    _iviName :: G.Name,
    _iviDefVal :: Maybe (G.Value Void),
    _iviType :: G.GType
  }
  deriving (Show, Eq, TH.Lift, Generic)

instance Cacheable InpValInfo

newtype SystemDefined = SystemDefined {unSystemDefined :: Bool}
  deriving (Show, Eq, FromJSON, ToJSON, PG.ToPrepArg, NFData, Cacheable)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined

data SQLGenCtx = SQLGenCtx
  { stringifyNum :: Options.StringifyNumbers,
    dangerousBooleanCollapse :: Options.DangerouslyCollapseBooleans,
    optimizePermissionFilters :: Options.OptimizePermissionFilters,
    bigqueryStringNumericInput :: Options.BigQueryStringNumericInput
  }
  deriving (Show, Eq)

successMsg :: EncJSON
successMsg = encJFromBuilder "{\"message\":\"success\"}"

newtype ResolvedWebhook = ResolvedWebhook {unResolvedWebhook :: Text}
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, ToTxt, Generic)

instance NFData ResolvedWebhook

instance Cacheable ResolvedWebhook

newtype InputWebhook = InputWebhook {unInputWebhook :: URLTemplate}
  deriving (Show, Eq, Generic)

instance NFData InputWebhook

instance Cacheable InputWebhook

instance Hashable InputWebhook

instance HasCodec InputWebhook where
  codec = dimapCodec InputWebhook unInputWebhook urlTemplateCodec
    where
      urlTemplateCodec =
        bimapCodec
          (mapLeft ("Parsing URL template failed: " ++) . parseURLTemplate)
          printURLTemplate
          codec

instance ToJSON InputWebhook where
  toJSON = String . printURLTemplate . unInputWebhook

instance FromJSON InputWebhook where
  parseJSON = withText "String" $ \t ->
    case parseURLTemplate t of
      Left e -> fail $ "Parsing URL template failed: " ++ e
      Right v -> pure $ InputWebhook v

instance PG.FromCol InputWebhook where
  fromCol bs = do
    urlTemplate <- parseURLTemplate <$> PG.fromCol bs
    bimap (\e -> "Parsing URL template failed: " <> T.pack e) InputWebhook urlTemplate

resolveWebhook :: QErrM m => Env.Environment -> InputWebhook -> m ResolvedWebhook
resolveWebhook env (InputWebhook urlTemplate) = do
  let eitherRenderedTemplate = renderURLTemplate env urlTemplate
  either
    (throw400 Unexpected . T.pack)
    (pure . ResolvedWebhook)
    eitherRenderedTemplate

newtype Timeout = Timeout {unTimeout :: Int}
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable)

instance FromJSON Timeout where
  parseJSON = withScientific "Timeout" $ \t -> do
    timeout <- onNothing (toBoundedInteger t) $ fail (show t <> " is out of bounds")
    case timeout >= 0 of
      True -> return $ Timeout timeout
      False -> fail "timeout value cannot be negative"

defaultActionTimeoutSecs :: Timeout
defaultActionTimeoutSecs = Timeout 30

-- | See API reference here:
--   https://hasura.io/docs/latest/graphql/core/api-reference/syntax-defs.html#pgconnectionparameters
data PGConnectionParams = PGConnectionParams
  { _pgcpHost :: Text,
    _pgcpUsername :: Text,
    _pgcpPassword :: Maybe Text,
    _pgcpPort :: Int,
    _pgcpDatabase :: Text
  }
  deriving (Show, Eq, Generic)

instance NFData PGConnectionParams

instance Cacheable PGConnectionParams

instance Hashable PGConnectionParams

instance HasCodec PGConnectionParams where
  codec =
    AC.object "PGConnectionParams" $
      PGConnectionParams
        <$> requiredField' "host"
        AC..= _pgcpHost
        <*> requiredField' "username"
        AC..= _pgcpUsername
        <*> optionalFieldOrNull' "password"
        AC..= _pgcpPassword
        <*> requiredField' "port"
        AC..= _pgcpPort
        <*> requiredField' "database"
        AC..= _pgcpDatabase

$(deriveToJSON hasuraJSON {omitNothingFields = True} ''PGConnectionParams)

instance FromJSON PGConnectionParams where
  parseJSON = withObject "PGConnectionParams" $ \o ->
    PGConnectionParams
      <$> o
      .: "host"
      <*> o
      .: "username"
      <*> o
      .:? "password"
      <*> o
      .: "port"
      <*> o
      .: "database"

data UrlConf
  = -- | the database connection string
    UrlValue InputWebhook
  | -- | the name of environment variable containing the connection string
    UrlFromEnv T.Text
  | -- | the minimum required `connection parameters` to construct a valid connection string
    UrlFromParams PGConnectionParams
  deriving (Show, Eq, Generic)

instance NFData UrlConf

instance Cacheable UrlConf

instance Hashable UrlConf

instance HasCodec UrlConf where
  codec =
    dimapCodec dec enc $
      disjointEitherCodec valCodec $
        disjointEitherCodec fromEnvCodec fromParamsCodec
    where
      valCodec = codec
      fromParamsCodec = AC.object "UrlConfFromParams" $ requiredField' "connection_parameters"

      dec (Left w) = UrlValue w
      dec (Right (Left wEnv)) = UrlFromEnv wEnv
      dec (Right (Right wParams)) = UrlFromParams wParams

      enc (UrlValue w) = Left w
      enc (UrlFromEnv wEnv) = Right $ Left wEnv
      enc (UrlFromParams wParams) = Right $ Right wParams

instance ToJSON UrlConf where
  toJSON (UrlValue w) = toJSON w
  toJSON (UrlFromEnv wEnv) = object ["from_env" .= wEnv]
  toJSON (UrlFromParams wParams) = object ["connection_parameters" .= wParams]

instance FromJSON UrlConf where
  parseJSON (Object o) = do
    mFromEnv <- (fmap . fmap) UrlFromEnv (o .:? "from_env")
    mFromParams <- (fmap . fmap) UrlFromParams (o .:? "connection_parameters")
    case (mFromEnv, mFromParams) of
      (Just fromEnv, Nothing) -> pure fromEnv
      (Nothing, Just fromParams) -> pure fromParams
      (Just _, Just _) -> fail $ commonJSONParseErrorMessage "Only one of "
      (Nothing, Nothing) -> fail $ commonJSONParseErrorMessage "Either "
    where
      -- NOTE(Sam): Maybe this could be put with other string manipulation utils
      -- helper to apply `dquote` for values of type `String`
      dquoteStr :: String -> String
      dquoteStr = T.unpack . dquote . T.pack

      -- helper for formatting error messages within this instance
      commonJSONParseErrorMessage :: String -> String
      commonJSONParseErrorMessage strToBePrepended =
        strToBePrepended
          <> dquoteStr "from_env"
          <> " or "
          <> dquoteStr "connection_parameters"
          <> " should be provided"
  parseJSON t@(String _) =
    case (fromJSON t) of
      Error s -> fail s
      Success a -> pure $ UrlValue a
  parseJSON _ = fail "one of string or object must be provided for url/webhook"

getConnOptionsFromConnParams :: PGConnectionParams -> PG.ConnOptions
getConnOptionsFromConnParams PGConnectionParams {..} =
  PG.ConnOptions
    { connHost = T.unpack _pgcpHost,
      connUser = T.unpack _pgcpUsername,
      connPort = _pgcpPort,
      connDatabase = T.unpack _pgcpDatabase,
      connPassword = T.unpack $ fromMaybe "" _pgcpPassword,
      connOptions = Nothing
    }

-- | Construct a Postgres connection URI as a String from 'PGConnectionParams'.
--
-- NOTE: This function takes care to properly escape all URI components, as
-- Postgres requires that a connection URI is percent-encoded if it includes
-- symbols with "special meaning".
--
-- See the @libpq@ documentation for details: https://www.postgresql.org/docs/13/libpq-connect.html#id-1.7.3.8.3.6
getPGConnectionStringFromParams :: PGConnectionParams -> String
getPGConnectionStringFromParams PGConnectionParams {..} =
  let uriAuth =
        rectifyAuth $
          URIAuth
            { uriUserInfo = getURIAuthUserInfo _pgcpUsername _pgcpPassword,
              uriRegName = unpackEscape _pgcpHost,
              uriPort = show _pgcpPort
            }
      pgConnectionURI =
        rectify $
          URI
            { uriScheme = "postgresql",
              uriAuthority = Just uriAuth,
              uriPath = "/" <> unpackEscape _pgcpDatabase,
              uriQuery = "",
              uriFragment = ""
            }
   in uriToString id pgConnectionURI $ "" -- NOTE: this is done because uriToString returns a value of type ShowS
  where
    -- Helper to manage proper escaping in URI components.
    unpackEscape = escapeURIString isUnescapedInURIComponent . T.unpack

    -- Construct the 'URIAuth' 'uriUserInfo' component string from a username
    -- and optional password provided by 'PGConnectionParams'.
    getURIAuthUserInfo :: Text -> Maybe Text -> String
    getURIAuthUserInfo username mPassword = case mPassword of
      Nothing -> unpackEscape username
      Just password -> unpackEscape username <> ":" <> unpackEscape password

resolveUrlConf :: MonadError QErr m => Env.Environment -> UrlConf -> m Text
resolveUrlConf env = \case
  UrlValue v -> unResolvedWebhook <$> resolveWebhook env v
  UrlFromEnv envVar -> getEnv env envVar
  UrlFromParams connParams ->
    pure . T.pack $ getPGConnectionStringFromParams connParams

getEnv :: QErrM m => Env.Environment -> Text -> m Text
getEnv env k = do
  let mEnv = Env.lookupEnv env (T.unpack k)
  case mEnv of
    Nothing -> throw400 NotFound $ "environment variable '" <> k <> "' not set"
    Just envVal -> return (T.pack envVal)

-- | Various user-controlled configuration for metrics used by Pro
data MetricsConfig = MetricsConfig
  { -- | should the query-variables be logged and analyzed for metrics
    _mcAnalyzeQueryVariables :: Bool,
    -- | should the response-body be analyzed for empty and null responses
    _mcAnalyzeResponseBody :: Bool
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''MetricsConfig)

emptyMetricsConfig :: MetricsConfig
emptyMetricsConfig = MetricsConfig False False

data Comment
  = -- | Automatically generate a comment (derive it from DB comments, or a sensible default describing the source of the data)
    Automatic
  | -- | The user's explicitly provided comment, or explicitly no comment (ie. leave it blank, do not autogenerate one)
    Explicit (Maybe NonEmptyText)
  deriving (Eq, Show, Generic)

instance NFData Comment

instance Cacheable Comment

instance Hashable Comment

instance HasCodec Comment where
  codec = dimapCodec dec enc (codec @(Maybe Text))
    where
      dec Nothing = Automatic
      dec (Just text) = Explicit $ mkNonEmptyText text

      enc Automatic = Nothing
      enc (Explicit (Just text)) = Just (toTxt text)
      enc (Explicit Nothing) = Just ""

instance FromJSON Comment where
  parseJSON = \case
    Null -> pure Automatic
    String text -> pure . Explicit $ mkNonEmptyText text
    val -> prependFailure "parsing Comment failed, " (typeMismatch "String or Null" val)

instance ToJSON Comment where
  toJSON Automatic = Null
  toJSON (Explicit (Just value)) = String (toTxt value)
  toJSON (Explicit Nothing) = String ""

commentToMaybeText :: Comment -> Maybe Text
commentToMaybeText Automatic = Nothing
commentToMaybeText (Explicit Nothing) = Just ""
commentToMaybeText (Explicit (Just val)) = Just (toTxt val)

commentFromMaybeText :: Maybe Text -> Comment
commentFromMaybeText Nothing = Automatic
commentFromMaybeText (Just val) = Explicit $ mkNonEmptyText val

-- | We use the following type, after we resolve the env var.
-- | This will store both the env var name and the resolved value.
data EnvRecord a = EnvRecord
  { _envVarName :: Text,
    _envVarValue :: a
  }
  deriving (Show, Eq, Generic)

instance NFData a => NFData (EnvRecord a)

instance Cacheable a => Cacheable (EnvRecord a)

instance Hashable a => Hashable (EnvRecord a)

instance (ToJSON a) => ToJSON (EnvRecord a) where
  toJSON (EnvRecord envVar _envValue) = object ["env_var" .= envVar]

instance (FromJSON a) => FromJSON (EnvRecord a)

data ApolloFederationVersion = V1 deriving (Show, Eq, Generic)

instance Cacheable ApolloFederationVersion

instance ToJSON ApolloFederationVersion where
  toJSON V1 = J.String "v1"

instance FromJSON ApolloFederationVersion where
  parseJSON = withText "ApolloFederationVersion" $
    \case
      "v1" -> pure V1
      _ -> fail "enable takes the version of apollo federation. Supported value is v1 only."

instance NFData ApolloFederationVersion

data ApolloFederationConfig = ApolloFederationConfig
  { enable :: ApolloFederationVersion
  }
  deriving (Show, Eq, Generic)

instance Cacheable ApolloFederationConfig

instance ToJSON ApolloFederationConfig

instance FromJSON ApolloFederationConfig

instance NFData ApolloFederationConfig

isApolloFedV1enabled :: Maybe ApolloFederationConfig -> Bool
isApolloFedV1enabled = isJust

--------------------------------------------------------------------------------
-- metadata

-- | Metadata representation of a generic remote relationship, regardless of the
-- source: all sources use this same agnostic definition. The internal
-- definition field is where we differentiate between different targets.
--
-- TODO: This needs to be moved to an appropriate module, maybe something
-- like Hasura.RemoteRelationships.Metadata.
data RemoteRelationshipG definition = RemoteRelationship
  { _rrName :: RelName,
    _rrDefinition :: definition
  }
  deriving (Show, Eq, Generic)

instance Cacheable definition => Cacheable (RemoteRelationshipG definition)

$(makeLenses ''RemoteRelationshipG)
$(deriveToJSON hasuraJSON {J.omitNothingFields = False} ''RemoteRelationshipG)
