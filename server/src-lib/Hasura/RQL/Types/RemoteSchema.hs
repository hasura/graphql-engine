module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude

import qualified Data.Aeson                     as J
import qualified Data.Aeson.TH                  as J
import qualified Data.Environment               as Env
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import qualified Database.PG.Query              as Q
import qualified Language.GraphQL.Draft.Printer as G
import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Network.URI.Extended           as N
import qualified Text.Builder                   as TB

import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.Base.Error
import           Hasura.GraphQL.Parser.Schema   (Variable)
import           Hasura.Incremental             (Cacheable)
import           Hasura.RQL.DDL.Headers         (HeaderConf (..))
import           Hasura.RQL.Types.Common
import           Hasura.Session


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

$(J.deriveJSON hasuraJSON ''RemoteSchemaInfo)

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
$(J.deriveToJSON hasuraJSON{J.omitNothingFields=True} ''RemoteSchemaDef)

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
$(J.deriveJSON hasuraJSON ''AddRemoteSchemaQuery)

newtype RemoteSchemaNameQuery
  = RemoteSchemaNameQuery
  { _rsnqName    :: RemoteSchemaName
  } deriving (Show, Eq)

$(J.deriveJSON hasuraJSON ''RemoteSchemaNameQuery)

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
$(J.deriveJSON hasuraJSON ''AddRemoteSchemaPermissions)

data DropRemoteSchemaPermissions
  = DropRemoteSchemaPermissions
  { _drspRemoteSchema :: !RemoteSchemaName
  , _drspRole         :: !RoleName
  } deriving (Show, Eq, Generic)
instance NFData DropRemoteSchemaPermissions
instance Cacheable DropRemoteSchemaPermissions
$(J.deriveJSON hasuraJSON ''DropRemoteSchemaPermissions)

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
  | RemoteJSONValue !G.GType !J.Value
  deriving (Show, Eq, Generic, Ord)
instance Hashable RemoteSchemaVariable
instance Cacheable RemoteSchemaVariable

-- | This data type is an extension of the `G.InputValueDefinition`, it
--   may contain a preset with it.
data RemoteSchemaInputValueDefinition
  = RemoteSchemaInputValueDefinition
  { _rsitdDefinition     :: !G.InputValueDefinition
  , _rsitdPresetArgument :: !(Maybe (G.Value RemoteSchemaVariable))
  } deriving (Show, Eq, Generic, Ord)
instance Hashable RemoteSchemaInputValueDefinition
instance Cacheable RemoteSchemaInputValueDefinition

newtype RemoteSchemaIntrospection
  = RemoteSchemaIntrospection [(G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)]
  deriving (Show, Eq, Generic, Hashable, Cacheable, Ord)

data RemoteFieldG var
  = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RemoteSchemaInfo
  , _rfField            :: !(G.Field G.NoFragments var)
  } deriving (Functor, Foldable, Traversable)

type RemoteField = RemoteFieldG RemoteSchemaVariable

data RemoteSchemaPermsCtx
  = RemoteSchemaPermsEnabled
  | RemoteSchemaPermsDisabled
  deriving (Show, Eq)

instance J.FromJSON RemoteSchemaPermsCtx where
  parseJSON = J.withBool "RemoteSchemaPermsCtx" $
    pure . bool RemoteSchemaPermsDisabled RemoteSchemaPermsEnabled

instance J.ToJSON RemoteSchemaPermsCtx where
  toJSON = \case
    RemoteSchemaPermsEnabled  -> J.Bool True
    RemoteSchemaPermsDisabled -> J.Bool False


lookupType
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
lookupType (RemoteSchemaIntrospection types) name = find (\tp -> getNamedTyp tp == name) types
  where
    getNamedTyp :: G.TypeDefinition possibleTypes RemoteSchemaInputValueDefinition -> G.Name
    getNamedTyp ty = case ty of
      G.TypeDefinitionScalar t      -> G._stdName t
      G.TypeDefinitionObject t      -> G._otdName t
      G.TypeDefinitionInterface t   -> G._itdName t
      G.TypeDefinitionUnion t       -> G._utdName t
      G.TypeDefinitionEnum t        -> G._etdName t
      G.TypeDefinitionInputObject t -> G._iotdName t

lookupObject
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
lookupObject (RemoteSchemaIntrospection types) name = choice $ types <&> \case
  G.TypeDefinitionObject t | G._otdName t == name -> Just t
  _                                               -> Nothing

lookupInterface
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe (G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
lookupInterface (RemoteSchemaIntrospection types) name = choice $ types <&> \case
  G.TypeDefinitionInterface t | G._itdName t == name -> Just t
  _                                                  -> Nothing

lookupScalar
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe G.ScalarTypeDefinition
lookupScalar (RemoteSchemaIntrospection types) name = choice $ types <&> \case
  G.TypeDefinitionScalar t | G._stdName t == name -> Just t
  _                                               -> Nothing

lookupUnion
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe G.UnionTypeDefinition
lookupUnion (RemoteSchemaIntrospection types) name = choice $ types <&> \case
  G.TypeDefinitionUnion t | G._utdName t == name -> Just t
  _                                              -> Nothing

lookupEnum
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe G.EnumTypeDefinition
lookupEnum (RemoteSchemaIntrospection types) name = choice $ types <&> \case
  G.TypeDefinitionEnum t | G._etdName t == name -> Just t
  _                                             -> Nothing

lookupInputObject
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe (G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition)
lookupInputObject (RemoteSchemaIntrospection types) name = choice $ types <&> \case
  G.TypeDefinitionInputObject t | G._iotdName t == name -> Just t
  _                                                     -> Nothing
