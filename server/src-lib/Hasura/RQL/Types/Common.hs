{-# LANGUAGE OverloadedLists #-}

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
    failureMsg,
    InputWebhook (..),
    ResolvedWebhook (..),
    ResolveWebhookError (..),
    resolveWebhook,
    resolveWebhookEither,
    Timeout (..),
    defaultActionTimeoutSecs,
    UrlConf (..),
    resolveUrlConf,
    getEnv,
    getEnvEither,
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
    remoteRelationshipCodec,
    rrDefinition,
    rrName,
    TriggerOnReplication (..),
  )
where

import Autodocodec
  ( HasCodec (codec),
    JSONCodec,
    bimapCodec,
    boundedIntegralCodec,
    dimapCodec,
    disjointEitherCodec,
    optionalFieldOrNull',
    requiredField,
    requiredField',
    requiredFieldWith',
    stringConstCodec,
  )
import Autodocodec qualified as AC
import Autodocodec.Extended (boolConstCodec, fromEnvCodec, typeableName)
import Control.Lens (Lens)
import Control.Lens qualified as Lens
import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Bifunctor (bimap)
import Data.Environment qualified as Env
import Data.List (isPrefixOf)
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Typeable (Typeable)
import Data.URL.Template
import Database.PG.Query qualified as PG
import Hasura.Base.Error
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.Instances ()
import Hasura.Base.ToErrorValue
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.Types.Schema.Options qualified as Options
import Language.GraphQL.Draft.Syntax qualified as G
import Language.Haskell.TH.Syntax qualified as TH
import Network.URI
import PostgreSQL.Binary.Decoding qualified as PD
import System.Directory (canonicalizePath)

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
      NFData
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

instance HasCodec RelType where
  codec =
    stringConstCodec
      [ (ObjRel, relTypeToTxt ObjRel),
        (ArrRel, relTypeToTxt ArrRel)
      ]

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array") = return ArrRel
  parseJSON _ = fail "expecting either 'object' or 'array' for rel_type"

instance PG.FromCol RelType where
  fromCol bs = flip PG.fromColHelper bs
    $ PD.enum
    $ \case
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
  deriving (Show, Eq, Ord, Generic)

instance NFData InsertOrder

instance Hashable InsertOrder

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
  deriving (Show, Eq, NFData, Hashable, ToJSON, FromJSON, PG.FromCol)

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
      Semigroup
    )

instance ToTxt FieldName where
  toTxt (FieldName c) = c

instance HasCodec FieldName where
  codec = dimapCodec FieldName getFieldNameTxt codec

-- The field name here is the GraphQL alias, i.e, the name with which the field
-- should appear in the response
type Fields a = [(FieldName, a)]

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data SourceName
  = SNDefault
  | SNName NonEmptyText
  deriving (Show, Eq, Ord, Generic)

sourceNameParser :: Text -> Parser SourceName
sourceNameParser = \case
  "default" -> pure SNDefault
  t -> SNName <$> parseJSON (String t)

instance FromJSON SourceName where
  parseJSON = withText "String" sourceNameParser

instance FromJSONKey SourceName

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

newtype SystemDefined = SystemDefined {unSystemDefined :: Bool}
  deriving (Show, Eq, FromJSON, ToJSON, PG.ToPrepArg, NFData, Generic)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined

data SQLGenCtx = SQLGenCtx
  { stringifyNum :: Options.StringifyNumbers,
    dangerousBooleanCollapse :: Options.DangerouslyCollapseBooleans,
    remoteNullForwardingPolicy :: Options.RemoteNullForwardingPolicy,
    optimizePermissionFilters :: Options.OptimizePermissionFilters,
    bigqueryStringNumericInput :: Options.BigQueryStringNumericInput
  }
  deriving (Show, Eq)

successMsg :: EncJSON
successMsg = encJFromBuilder "{\"message\":\"success\"}"

failureMsg :: EncJSON
failureMsg = encJFromBuilder "{\"message\":\"failure\"}"

newtype ResolvedWebhook = ResolvedWebhook {unResolvedWebhook :: Text}
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, ToTxt, Generic)

instance NFData ResolvedWebhook

-- TODO this seems to be a "URL template"; rename
newtype InputWebhook = InputWebhook {unInputWebhook :: Template}
  deriving (Show, Eq, Generic)

instance NFData InputWebhook

instance Hashable InputWebhook

instance HasCodec InputWebhook where
  codec = dimapCodec InputWebhook unInputWebhook urlTemplateCodec
    where
      urlTemplateCodec =
        bimapCodec
          (mapLeft ("Parsing URL template failed: " ++) . parseTemplate)
          printTemplate
          codec

instance ToJSON InputWebhook where
  toJSON = String . printTemplate . unInputWebhook

instance FromJSON InputWebhook where
  parseJSON = withText "String" $ \t ->
    case parseTemplate t of
      Left e -> fail $ "Parsing URL template failed: " ++ e
      Right v -> pure $ InputWebhook v

instance PG.FromCol InputWebhook where
  fromCol bs = do
    urlTemplate <- parseTemplate <$> PG.fromCol bs
    bimap (\e -> "Parsing URL template failed: " <> T.pack e) InputWebhook urlTemplate

-- Consists of the environment variable name with missing/invalid value
newtype ResolveWebhookError = ResolveWebhookError {unResolveWebhookError :: Text} deriving (Show, ToTxt)

resolveWebhook :: (QErrM m) => Env.Environment -> InputWebhook -> m ResolvedWebhook
resolveWebhook env inputWebhook = do
  let eitherRenderedTemplate = resolveWebhookEither env inputWebhook
  onLeft
    eitherRenderedTemplate
    (throw400 Unexpected . ("Value for environment variables not found: " <>) . unResolveWebhookError)

-- This is similar to `resolveWebhook` but it doesn't fail when an env var is invalid
resolveWebhookEither :: Env.Environment -> InputWebhook -> Either ResolveWebhookError ResolvedWebhook
resolveWebhookEither env (InputWebhook urlTemplate) =
  bimap ResolveWebhookError ResolvedWebhook (renderTemplate env urlTemplate)

newtype Timeout = Timeout {unTimeout :: Int}
  deriving (Show, Eq, ToJSON, Generic, NFData)

instance HasCodec Timeout where
  codec = bimapCodec dec enc boundedIntegralCodec
    where
      dec timeout = case timeout >= 0 of
        True -> Right $ Timeout timeout
        False -> Left "timeout value cannot be negative"
      enc (Timeout n) = n

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

instance Hashable PGConnectionParams

instance HasCodec PGConnectionParams where
  codec =
    AC.object "PGConnectionParams"
      $ PGConnectionParams
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

-- TODO: Use HasCodec to define Aeson instances?
instance ToJSON PGConnectionParams where
  toJSON PGConnectionParams {..} =
    J.object
      $ [ "host" .= _pgcpHost,
          "username" .= _pgcpUsername,
          "port" .= _pgcpPort,
          "database" .= _pgcpDatabase
        ]
      ++ ["password" .= _pgcpPassword | isJust _pgcpPassword]

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

-- | user PG connection configuration from which we'll eventually make a 'ConnInfo'
data UrlConf
  = -- | the database connection string
    UrlValue InputWebhook
  | -- | the name of environment variable containing the connection string
    UrlFromEnv T.Text
  | -- | the minimum required `connection parameters` to construct a valid connection string
    UrlFromParams PGConnectionParams
  | -- | Filepath to a file containing a connection string. This is read
    -- before each connect, and when in use connection errors will force a
    -- re-read. This can support e.g. environments where passwords are
    -- frequently rotated (not supported on cloud)
    --
    -- This gets piped all the way through into the ConnInfo.
    UrlDynamicFromFile FilePath
  deriving (Show, Eq, Generic)

instance NFData UrlConf

instance Hashable UrlConf

instance HasCodec UrlConf where
  codec =
    dimapCodec dec enc
      $ disjointEitherCodec valCodec
      $ disjointEitherCodec fromEnvCodec fromParamsCodec
    where
      valCodec = codec
      fromParamsCodec = AC.object "UrlConfFromParams" $ requiredField' "connection_parameters"

      dec (Left w) = UrlValue w
      dec (Right (Left wEnv)) = UrlFromEnv wEnv
      dec (Right (Right (Left wParams))) = UrlFromParams wParams
      dec (Right (Right (Right wPath))) = UrlDynamicFromFile wPath

      enc (UrlValue w) = Left w
      enc (UrlFromEnv wEnv) = Right $ Left wEnv
      enc (UrlFromParams wParams) = Right $ Right $ Left wParams
      enc (UrlDynamicFromFile wPath) = Right $ Right $ Right wPath

instance ToJSON UrlConf where
  toJSON (UrlValue w) = toJSON w
  toJSON (UrlFromEnv wEnv) = object ["from_env" .= wEnv]
  toJSON (UrlFromParams wParams) = object ["connection_parameters" .= wParams]
  toJSON (UrlDynamicFromFile wPath) = object ["dynamic_from_file" .= wPath]

instance FromJSON UrlConf where
  parseJSON (Object o) = do
    mFromEnv <- (fmap . fmap) UrlFromEnv (o .:? "from_env")
    mDynamicFromFile <- (fmap . fmap) UrlDynamicFromFile (o .:? "dynamic_from_file")
    mFromParams <- (fmap . fmap) UrlFromParams (o .:? "connection_parameters")
    case (mFromEnv, mFromParams, mDynamicFromFile) of
      (Just fromEnv, Nothing, Nothing) -> pure fromEnv
      (Nothing, Just fromParams, Nothing) -> pure fromParams
      (Nothing, Nothing, Just dynamicFromFile) -> pure dynamicFromFile
      (Nothing, Nothing, Nothing) -> fail $ commonJSONParseErrorMessage "Either "
      (_, _, _) -> fail $ commonJSONParseErrorMessage "Only one of "
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
        rectifyAuth
          $ URIAuth
            { uriUserInfo = getURIAuthUserInfo _pgcpUsername _pgcpPassword,
              uriRegName = unpackEscape _pgcpHost,
              uriPort = show _pgcpPort
            }
      pgConnectionURI =
        rectify
          $ URI
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

-- | NOTE: Because hasura admins are not necessarily trusted to be able to read
-- arbitrary files on the machine running the server, we insist the file path
-- supplied by the hasura admin be validated against an acceptable prefix
-- (set only by an env var,  presumably only by someone with privileges to
-- deploy).
--
-- Altering the value of HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX after
-- adding a dynamic source can result in inconsistent metadata.
resolveUrlConf :: (MonadIO m, MonadError QErr m) => Env.Environment -> UrlConf -> m PG.ConnDetails
resolveUrlConf env = \case
  UrlValue v -> toURI . unResolvedWebhook <$> resolveWebhook env v
  UrlFromEnv envVar -> toURI <$> getEnv env envVar
  UrlFromParams connParams ->
    pure . toURI . T.pack $ getPGConnectionStringFromParams connParams
  UrlDynamicFromFile fpathDirty -> do
    fpath <- case Env.lookupEnv env "HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX" of
      Nothing -> throw400 PermissionError $ "dynamic_from_file file path requires that the HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX environment variable be set and non-empty"
      -- Since this might be an accidental misconfiguration:
      Just "" -> throw400 PermissionError $ "dynamic_from_file file path requires that the HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX environment variable be non-empty"
      -- Canonicalize the supplied (untrusted) file path, in an
      -- attempt to prevent escapes (like `..`).  canonicalize both
      -- path  and allowed prefix, so that matching is robust.
      Just allowedPrefixNonCanon -> do
        allowedPrefixCanon <- liftIO $ canonicalizePath allowedPrefixNonCanon
        fpathDirtyCanon <- liftIO $ canonicalizePath fpathDirty
        if allowedPrefixCanon `isPrefixOf` fpathDirtyCanon
          then pure fpathDirty
          else -- I guess we'll avoid leaking info here too...
            throw400 PermissionError $ "The supplied dynamic_from_file file path, when canonicalized, does not match the allowed prefix set by your administrator via the HASURA_GRAPHQL_DYNAMIC_SECRETS_ALLOWED_PATH_PREFIX environment variable"
    pure $ PG.CDDynamicDatabaseURI fpath
  where
    toURI = PG.CDDatabaseURI . txtToBs

getEnv :: (QErrM m) => Env.Environment -> Text -> m Text
getEnv env k = do
  let eitherEnv = getEnvEither env k
  onLeft
    eitherEnv
    (\_ -> throw400 NotFound $ "environment variable '" <> k <> "' not set")

-- This is similar to `getEnv` but it doesn't fail when the env var is invalid
getEnvEither :: Env.Environment -> Text -> Either Text Text
getEnvEither env k =
  case Env.lookupEnv env (T.unpack k) of
    Nothing -> Left k
    Just envVal -> Right (T.pack envVal)

-- | Various user-controlled configuration for metrics used by Pro
data MetricsConfig = MetricsConfig
  { -- | should the query-variables be logged and analyzed for metrics
    _mcAnalyzeQueryVariables :: Bool,
    -- | should the response-body be analyzed for empty and null responses
    _mcAnalyzeResponseBody :: Bool
  }
  deriving (Show, Eq, Generic)

instance HasCodec MetricsConfig where
  codec =
    AC.object "MetricsConfig"
      $ MetricsConfig
      <$> requiredField' "analyze_query_variables"
      AC..= _mcAnalyzeQueryVariables
        <*> requiredField' "analyze_response_body"
      AC..= _mcAnalyzeResponseBody

instance FromJSON MetricsConfig where
  parseJSON = J.withObject "MetricsConfig" $ \o -> do
    _mcAnalyzeQueryVariables <- o .: "analyze_query_variables"
    _mcAnalyzeResponseBody <- o .: "analyze_response_body"
    pure MetricsConfig {..}

instance ToJSON MetricsConfig where
  toJSON MetricsConfig {..} =
    J.object
      [ "analyze_query_variables" .= _mcAnalyzeQueryVariables,
        "analyze_response_body" .= _mcAnalyzeResponseBody
      ]

emptyMetricsConfig :: MetricsConfig
emptyMetricsConfig = MetricsConfig False False

data Comment
  = -- | Automatically generate a comment (derive it from DB comments, or a sensible default describing the source of the data)
    Automatic
  | -- | The user's explicitly provided comment, or explicitly no comment (ie. leave it blank, do not autogenerate one)
    Explicit (Maybe NonEmptyText)
  deriving (Eq, Show, Generic)

instance NFData Comment

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

instance (NFData a) => NFData (EnvRecord a)

instance (Hashable a) => Hashable (EnvRecord a)

instance (ToJSON a) => ToJSON (EnvRecord a) where
  toJSON (EnvRecord envVar _envValue) = object ["env_var" .= envVar]

instance (FromJSON a) => FromJSON (EnvRecord a)

data ApolloFederationVersion = V1 deriving (Show, Eq, Generic)

instance HasCodec ApolloFederationVersion where
  codec = stringConstCodec [(V1, "v1")]

instance ToJSON ApolloFederationVersion where
  toJSON V1 = J.String "v1"

instance FromJSON ApolloFederationVersion where
  parseJSON = withText "ApolloFederationVersion"
    $ \case
      "v1" -> pure V1
      _ -> fail "enable takes the version of apollo federation. Supported value is v1 only."

instance NFData ApolloFederationVersion

data ApolloFederationConfig = ApolloFederationConfig
  { enable :: ApolloFederationVersion
  }
  deriving (Show, Eq, Generic)

instance HasCodec ApolloFederationConfig where
  codec =
    AC.object "ApolloFederationConfig"
      $ ApolloFederationConfig
      <$> requiredField "enable" enableDoc
      AC..= enable
    where
      enableDoc = "enable takes the version of apollo federation. Supported value is v1 only."

instance ToJSON ApolloFederationConfig

instance FromJSON ApolloFederationConfig

instance NFData ApolloFederationConfig

isApolloFedV1enabled :: Maybe ApolloFederationConfig -> Bool
isApolloFedV1enabled = isJust

-- | Type to indicate if the SQL trigger should be enabled
--   when data is inserted into a table through replication.
data TriggerOnReplication
  = TOREnableTrigger
  | TORDisableTrigger
  deriving (Show, Eq, Generic)

instance NFData TriggerOnReplication

instance HasCodec TriggerOnReplication where
  codec = boolConstCodec TOREnableTrigger TORDisableTrigger

instance FromJSON TriggerOnReplication where
  parseJSON = withBool "TriggerOnReplication" $ \case
    True -> pure TOREnableTrigger
    False -> pure TORDisableTrigger

instance ToJSON TriggerOnReplication where
  toJSON = \case
    TOREnableTrigger -> Bool True
    TORDisableTrigger -> Bool False

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

instance (ToJSON definition) => ToJSON (RemoteRelationshipG definition) where
  toJSON RemoteRelationship {..} =
    J.object
      [ "name" .= _rrName,
        "definition" .= _rrDefinition
      ]

rrName :: Lens (RemoteRelationshipG def) (RemoteRelationshipG def) RelName RelName
rrName = Lens.lens _rrName (\rrg a -> rrg {_rrName = a})

rrDefinition :: Lens (RemoteRelationshipG def) (RemoteRelationshipG def') def def'
rrDefinition = Lens.lens _rrDefinition (\rrg a -> rrg {_rrDefinition = a})

remoteRelationshipCodec ::
  forall definition.
  (Typeable definition) =>
  JSONCodec definition ->
  JSONCodec (RemoteRelationshipG definition)
remoteRelationshipCodec definitionCodec =
  AC.object ("RemoteRelationship_" <> typeableName @definition)
    $ RemoteRelationship
    <$> requiredField' "name"
    AC..= _rrName
      <*> requiredFieldWith' "definition" definitionCodec
    AC..= _rrDefinition
