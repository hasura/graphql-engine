module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude

import qualified Data.Aeson                     as J
import qualified Data.Aeson.Casing              as J
import qualified Data.Aeson.TH                  as J
import qualified Data.Environment               as Env
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import qualified Text.Builder                   as TB
import           Data.Text.Extended
import           Data.Text.NonEmpty
import qualified Database.PG.Query              as Q
import qualified Network.URI.Extended           as N
import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Language.GraphQL.Draft.Printer as G

import           Hasura.Incremental         (Cacheable)
import           Hasura.RQL.DDL.Headers     (HeaderConf (..))
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.Session
import           Hasura.GraphQL.Parser.Schema      (Variable)

type UrlFromEnv = Text

-- | Remote schema identifier.
--
-- NOTE: no validation on the character set is done here; it's likely there is
-- a bug (FIXME) where this interacts with remote relationships and some name
-- mangling needs to happen.
newtype RemoteSchemaName
  = RemoteSchemaName
  { unRemoteSchemaName :: NonEmptyText }
  deriving ( Show, Eq, Ord, Hashable, J.ToJSON, J.ToJSONKey
           , J.FromJSON, Q.ToPrepArg, Q.FromCol, ToTxt, NFData
           , Generic, Cacheable, Arbitrary
           )

-- | 'RemoteSchemaDef' after validation and baking-in of defaults in 'validateRemoteSchemaDef'.
data RemoteSchemaInfo
  = RemoteSchemaInfo
  { rsUrl              :: !N.URI
  , rsHeaders          :: ![HeaderConf]
  , rsFwdClientHeaders :: !Bool
  , rsTimeoutSeconds   :: !Int
  } deriving (Show, Eq, Generic)
instance NFData RemoteSchemaInfo
instance Cacheable RemoteSchemaInfo
instance Hashable RemoteSchemaInfo

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''RemoteSchemaInfo)

-- | From the user's API request
data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsdUrl                  :: !(Maybe InputWebhook)
  , _rsdUrlFromEnv           :: !(Maybe UrlFromEnv)
  , _rsdHeaders              :: !(Maybe [HeaderConf])
  , _rsdForwardClientHeaders :: !Bool
  , _rsdTimeoutSeconds       :: !(Maybe Int)
  } deriving (Show, Eq, Generic)
instance NFData RemoteSchemaDef
instance Cacheable RemoteSchemaDef
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase){J.omitNothingFields=True} ''RemoteSchemaDef)

instance J.FromJSON RemoteSchemaDef where
  parseJSON = J.withObject "Object" $ \o ->
    RemoteSchemaDef
      <$> o J..:? "url"
      <*> o J..:? "url_from_env"
      <*> o J..:? "headers"
      <*> o J..:? "forward_client_headers" J..!= False
      <*> o J..:? "timeout_seconds"

-- | The payload for 'add_remote_schema', and a component of 'Metadata'.
data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqName       :: !RemoteSchemaName
  -- ^ An internal identifier for this remote schema.
  , _arsqDefinition :: !RemoteSchemaDef
  , _arsqComment    :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData AddRemoteSchemaQuery
instance Cacheable AddRemoteSchemaQuery
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)

newtype RemoteSchemaNameQuery
  = RemoteSchemaNameQuery
  { _rsnqName    :: RemoteSchemaName
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoteSchemaNameQuery)

getUrlFromEnv :: (MonadIO m, MonadError QErr m) => Env.Environment -> Text -> m N.URI
getUrlFromEnv env urlFromEnv = do
  let mEnv = Env.lookupEnv env $ T.unpack urlFromEnv
  uri <- onNothing mEnv (throw400 InvalidParams $ envNotFoundMsg urlFromEnv)
  onNothing (N.parseURI uri) (throw400 InvalidParams $ invalidUri uri)
  where
    invalidUri x = "not a valid URI: " <> T.pack x
    envNotFoundMsg e = "environment variable '" <> e <> "' not set"

validateRemoteSchemaDef
  :: (MonadError QErr m, MonadIO m)
  => Env.Environment
  -> RemoteSchemaDef
  -> m RemoteSchemaInfo
validateRemoteSchemaDef env (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs mTimeout) =
  case (mUrl, mUrlEnv) of
    (Just url, Nothing)    -> do
      resolvedWebhookTxt <- unResolvedWebhook <$> resolveWebhook env url
      case N.parseURI $ T.unpack resolvedWebhookTxt of
        Nothing  -> throw400 InvalidParams $ "not a valid URI: " <> resolvedWebhookTxt
        Just uri -> return $ RemoteSchemaInfo uri hdrs fwdHdrs timeout
    (Nothing, Just urlEnv) -> do
      url <- getUrlFromEnv env urlEnv
      return $ RemoteSchemaInfo url hdrs fwdHdrs timeout
    (Nothing, Nothing)     ->
        throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    (Just _, Just _)       ->
        throw400 InvalidParams "both `url` and `url_from_env` can't be present"
  where
    hdrs = fromMaybe [] hdrC

    timeout = fromMaybe 60 mTimeout

newtype RemoteSchemaPermissionDefinition
  = RemoteSchemaPermissionDefinition
  { _rspdSchema    :: G.SchemaDocument
  }  deriving (Show, Eq, Generic)
instance NFData RemoteSchemaPermissionDefinition
instance Cacheable RemoteSchemaPermissionDefinition
instance Hashable RemoteSchemaPermissionDefinition

instance J.FromJSON RemoteSchemaPermissionDefinition where
  parseJSON = J.withObject "RemoteSchemaPermissionDefinition" $ \obj -> do
    fmap RemoteSchemaPermissionDefinition $ obj J..: "schema"

instance J.ToJSON RemoteSchemaPermissionDefinition where
  toJSON (RemoteSchemaPermissionDefinition schema) =
    J.object $ [ "schema" J..= J.String (TB.run . G.schemaDocument $ schema)]

data AddRemoteSchemaPermissions
  = AddRemoteSchemaPermissions
  { _arspRemoteSchema :: !RemoteSchemaName
  , _arspRole         :: !RoleName
  , _arspDefinition   :: !RemoteSchemaPermissionDefinition
  , _arspComment      :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData AddRemoteSchemaPermissions
instance Cacheable AddRemoteSchemaPermissions
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaPermissions)

data DropRemoteSchemaPermissions
  = DropRemoteSchemaPermissions
  { _drspRemoteSchema :: !RemoteSchemaName
  , _drspRole         :: !RoleName
  } deriving (Show, Eq, Generic)
instance NFData DropRemoteSchemaPermissions
instance Cacheable DropRemoteSchemaPermissions
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''DropRemoteSchemaPermissions)

-- | See `resolveRemoteVariable` function. This data type is used
--   for validation of the session variable value
data SessionArgumentPresetInfo
  = SessionArgumentPresetScalar
  | SessionArgumentPresetEnum !(Set.HashSet G.EnumValue)
  deriving (Show, Eq, Generic, Ord)
instance Hashable SessionArgumentPresetInfo
instance Cacheable SessionArgumentPresetInfo

-- | RemoteSchemaVariable is used to capture all the details required
--   to resolve a session preset variable.
--   See Note [Remote Schema Permissions Architecture]
data RemoteSchemaVariable
  = SessionPresetVariable !SessionVariable !G.Name !SessionArgumentPresetInfo
  | QueryVariable !Variable
  deriving (Show, Eq, Generic, Ord)
instance Hashable RemoteSchemaVariable
instance Cacheable RemoteSchemaVariable

-- | This data type is an extension of the `G.InputValueDefinition`, it
--   may contain a preset with it.
data RemoteSchemaInputValueDefinition
  = RemoteSchemaInputValueDefinition
  { _rsitdDefinition      :: !G.InputValueDefinition
  , _rsitdPresetArgument  :: !(Maybe (G.Value RemoteSchemaVariable))
  } deriving (Show, Eq, Generic, Ord)
instance Hashable RemoteSchemaInputValueDefinition
instance Cacheable RemoteSchemaInputValueDefinition

newtype RemoteSchemaIntrospection
  = RemoteSchemaIntrospection [(G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)]
  deriving (Show, Eq, Generic, Hashable, Cacheable, Ord)
