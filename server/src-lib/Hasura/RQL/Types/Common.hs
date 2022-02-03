module Hasura.RQL.Types.Common
  ( RelName (..),
    relNameToTxt,
    fromRemoteRelationship,
    RelType (..),
    relTypeToTxt,
    OID (..),
    FieldName (..),
    InsertOrder (..),
    ToAesonPairs (..),
    InpValInfo (..),
    SystemDefined (..),
    isSystemDefined,
    SQLGenCtx (..),
    successMsg,
    NonNegativeDiffTime,
    unNonNegativeDiffTime,
    unsafeNonNegativeDiffTime,
    mkNonNegativeDiffTime,
    InputWebhook (..),
    ResolvedWebhook (..),
    resolveWebhook,
    NonNegativeInt,
    getNonNegativeInt,
    mkNonNegativeInt,
    unsafeNonNegativeInt,
    Timeout (..),
    defaultActionTimeoutSecs,
    UrlConf (..),
    resolveUrlConf,
    getEnv,
    SourceName (..),
    defaultSource,
    sourceNameToText,
    JsonAggSelect (..),
    intScalar,
    floatScalar,
    stringScalar,
    boolScalar,
    idScalar,
    MetricsConfig (..),
    emptyMetricsConfig,
    PGConnectionParams (..),
    getPGConnectionStringFromParams,
    getConnOptionsFromConnParams,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Bifunctor (bimap)
import Data.Environment qualified as Env
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.URL.Template
import Database.PG.Query qualified as Q
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Incremental (Cacheable)
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
      Q.ToPrepArg,
      Q.FromCol,
      Generic,
      NFData,
      Cacheable
    )

instance ToTxt RelName where
  toTxt = relNameToTxt

relNameToTxt :: RelName -> Text
relNameToTxt = unNonEmptyText . getRelTxt

fromRemoteRelationship :: RelName -> FieldName
fromRemoteRelationship = FieldName . relNameToTxt

data RelType
  = ObjRel
  | ArrRel
  deriving (Show, Eq, Generic, Data)

instance NFData RelType

instance Hashable RelType

instance Cacheable RelType

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array") = return ArrRel
  parseJSON _ = fail "expecting either 'object' or 'array' for rel_type"

instance Q.FromCol RelType where
  fromCol bs = flip Q.fromColHelper bs $
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
  deriving (Show, Eq, NFData, Hashable, ToJSON, FromJSON, Q.FromCol, Cacheable)

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

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data SourceName
  = SNDefault
  | SNName !NonEmptyText
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SourceName where
  parseJSON = withText "String" $ \case
    "default" -> pure SNDefault
    t -> SNName <$> parseJSON (String t)

sourceNameToText :: SourceName -> Text
sourceNameToText = \case
  SNDefault -> "default"
  SNName t -> unNonEmptyText t

instance ToJSON SourceName where
  toJSON = String . sourceNameToText

instance ToTxt SourceName where
  toTxt = sourceNameToText

instance ToJSONKey SourceName

instance Hashable SourceName

instance NFData SourceName

instance Cacheable SourceName

defaultSource :: SourceName
defaultSource = SNDefault

data InpValInfo = InpValInfo
  { _iviDesc :: !(Maybe G.Description),
    _iviName :: !G.Name,
    _iviDefVal :: !(Maybe (G.Value Void)),
    _iviType :: !G.GType
  }
  deriving (Show, Eq, TH.Lift, Generic)

instance Cacheable InpValInfo

newtype SystemDefined = SystemDefined {unSystemDefined :: Bool}
  deriving (Show, Eq, FromJSON, ToJSON, Q.ToPrepArg, NFData, Cacheable)

isSystemDefined :: SystemDefined -> Bool
isSystemDefined = unSystemDefined

data SQLGenCtx = SQLGenCtx
  { stringifyNum :: Bool,
    dangerousBooleanCollapse :: Bool,
    optimizePermissionFilters :: Bool
  }
  deriving (Show, Eq)

successMsg :: EncJSON
successMsg = "{\"message\":\"success\"}"

newtype NonNegativeInt = NonNegativeInt {getNonNegativeInt :: Int}
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Num)

mkNonNegativeInt :: Int -> Maybe NonNegativeInt
mkNonNegativeInt x = case x >= 0 of
  True -> Just $ NonNegativeInt x
  False -> Nothing

unsafeNonNegativeInt :: Int -> NonNegativeInt
unsafeNonNegativeInt = NonNegativeInt

instance FromJSON NonNegativeInt where
  parseJSON = withScientific "NonNegativeInt" $ \t -> do
    case t >= 0 of
      True -> maybe (fail "integer passed is out of bounds") (pure . NonNegativeInt) $ toBoundedInteger t
      False -> fail "negative value not allowed"

newtype NonNegativeDiffTime = NonNegativeDiffTime {unNonNegativeDiffTime :: DiffTime}
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Num)

unsafeNonNegativeDiffTime :: DiffTime -> NonNegativeDiffTime
unsafeNonNegativeDiffTime = NonNegativeDiffTime

mkNonNegativeDiffTime :: DiffTime -> Maybe NonNegativeDiffTime
mkNonNegativeDiffTime x = case x >= 0 of
  True -> Just $ NonNegativeDiffTime x
  False -> Nothing

instance FromJSON NonNegativeDiffTime where
  parseJSON = withScientific "NonNegativeDiffTime" $ \t -> do
    case t >= 0 of
      True -> return $ NonNegativeDiffTime . realToFrac $ t
      False -> fail "negative value not allowed"

newtype ResolvedWebhook = ResolvedWebhook {unResolvedWebhook :: Text}
  deriving (Show, Eq, FromJSON, ToJSON, Hashable, ToTxt, Generic)

instance NFData ResolvedWebhook

instance Cacheable ResolvedWebhook

newtype InputWebhook = InputWebhook {unInputWebhook :: URLTemplate}
  deriving (Show, Eq, Generic)

instance NFData InputWebhook

instance Cacheable InputWebhook

instance Hashable InputWebhook

instance ToJSON InputWebhook where
  toJSON = String . printURLTemplate . unInputWebhook

instance FromJSON InputWebhook where
  parseJSON = withText "String" $ \t ->
    case parseURLTemplate t of
      Left e -> fail $ "Parsing URL template failed: " ++ e
      Right v -> pure $ InputWebhook v

instance Q.FromCol InputWebhook where
  fromCol bs = do
    urlTemplate <- parseURLTemplate <$> Q.fromCol bs
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
  { _pgcpHost :: !Text,
    _pgcpUsername :: !Text,
    _pgcpPassword :: !(Maybe Text),
    _pgcpPort :: !Int,
    _pgcpDatabase :: !Text
  }
  deriving (Show, Eq, Generic)

instance NFData PGConnectionParams

instance Cacheable PGConnectionParams

instance Hashable PGConnectionParams

$(makeLenses ''PGConnectionParams)
$(deriveToJSON hasuraJSON {omitNothingFields = True} ''PGConnectionParams)

instance FromJSON PGConnectionParams where
  parseJSON = withObject "PGConnectionParams" $ \o ->
    PGConnectionParams
      <$> o .: "host"
      <*> o .: "username"
      <*> o .:? "password"
      <*> o .: "port"
      <*> o .: "database"

data UrlConf
  = -- | the database connection string
    UrlValue !InputWebhook
  | -- | the name of environment variable containing the connection string
    UrlFromEnv !T.Text
  | -- | the minimum required `connection parameters` to construct a valid connection string
    UrlFromParams !PGConnectionParams
  deriving (Show, Eq, Generic)

instance NFData UrlConf

instance Cacheable UrlConf

instance Hashable UrlConf

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
        strToBePrepended <> dquoteStr "from_env" <> " or "
          <> dquoteStr "connection_parameters"
          <> " should be provided"
  parseJSON t@(String _) =
    case (fromJSON t) of
      Error s -> fail s
      Success a -> pure $ UrlValue a
  parseJSON _ = fail "one of string or object must be provided for url/webhook"

getConnOptionsFromConnParams :: PGConnectionParams -> Q.ConnOptions
getConnOptionsFromConnParams PGConnectionParams {..} =
  Q.ConnOptions
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

-- default scalar names
intScalar, floatScalar, stringScalar, boolScalar, idScalar :: G.Name
intScalar = $$(G.litName "Int")
floatScalar = $$(G.litName "Float")
stringScalar = $$(G.litName "String")
boolScalar = $$(G.litName "Boolean")
idScalar = $$(G.litName "ID")

-- | Various user-controlled configuration for metrics used by Pro
data MetricsConfig = MetricsConfig
  { -- | should the query-variables be logged and analyzed for metrics
    _mcAnalyzeQueryVariables :: !Bool,
    -- | should the response-body be analyzed for empty and null responses
    _mcAnalyzeResponseBody :: !Bool
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonPrefix snakeCase) ''MetricsConfig)

emptyMetricsConfig :: MetricsConfig
emptyMetricsConfig = MetricsConfig False False
