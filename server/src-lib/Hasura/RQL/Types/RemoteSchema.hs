module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Casing             as J
import qualified Data.Aeson.TH                 as J
import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Network.URI.Extended          as N
import qualified Data.Environment              as Env
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.Incremental         (Cacheable)
import           Hasura.RQL.DDL.Headers     (HeaderConf (..))
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Types
import           Hasura.Session
import           Hasura.GraphQL.Parser.Schema      (Variable)

type UrlFromEnv = Text

newtype RemoteSchemaName
  = RemoteSchemaName
  { unRemoteSchemaName :: NonEmptyText }
  deriving ( Show, Eq, Ord, Lift, Hashable, J.ToJSON, J.ToJSONKey
           , J.FromJSON, Q.ToPrepArg, Q.FromCol, DQuote, NFData
           , Generic, Cacheable, Arbitrary
           )

data RemoteSchemaInfo
  = RemoteSchemaInfo
  { rsUrl              :: !N.URI
  , rsHeaders          :: ![HeaderConf]
  , rsFwdClientHeaders :: !Bool
  , rsTimeoutSeconds   :: !Int
  } deriving (Show, Eq, Lift, Generic)
instance NFData RemoteSchemaInfo
instance Cacheable RemoteSchemaInfo
instance Hashable RemoteSchemaInfo

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''RemoteSchemaInfo)

data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsdUrl                  :: !(Maybe InputWebhook)
  , _rsdUrlFromEnv           :: !(Maybe UrlFromEnv)
  , _rsdHeaders              :: !(Maybe [HeaderConf])
  , _rsdForwardClientHeaders :: !Bool
  , _rsdTimeoutSeconds       :: !(Maybe Int)
  } deriving (Show, Eq, Lift, Generic)
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

data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqName       :: !RemoteSchemaName
  , _arsqDefinition :: !RemoteSchemaDef
  , _arsqComment    :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData AddRemoteSchemaQuery
instance Cacheable AddRemoteSchemaQuery
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaQuery)

newtype RemoteSchemaNameQuery
  = RemoteSchemaNameQuery
  { _rsnqName    :: RemoteSchemaName
  } deriving (Show, Eq, Lift)

$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''RemoteSchemaNameQuery)

getUrlFromEnv :: (MonadIO m, MonadError QErr m) => Env.Environment -> Text -> m N.URI
getUrlFromEnv env urlFromEnv = do
  let mEnv = Env.lookupEnv env $ T.unpack urlFromEnv
  uri <- maybe (throw400 InvalidParams $ envNotFoundMsg urlFromEnv) return mEnv
  maybe (throw400 InvalidParams $ invalidUri uri) return $ N.parseURI uri
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

data RemoteSchemaPermissionDefinition
  = RemoteSchemaPermissionDefinition
  { _rspdSchema    :: !G.SchemaDocument
    -- FIXME: not sure, if this is a hack (to store the "raw" schema) in a Text field
    -- rather than generating the `schema` again from `_rspdSchema`. Even, if it were
    -- to be done, the result will be exactly the same!
  , _rspdRawSchema :: !Text
  }  deriving (Show, Eq, Lift, Generic)
instance NFData RemoteSchemaPermissionDefinition
instance Cacheable RemoteSchemaPermissionDefinition
instance Hashable RemoteSchemaPermissionDefinition

instance J.FromJSON RemoteSchemaPermissionDefinition where
  parseJSON = J.withObject "RemoteSchemaPermissionDefinition" $ \obj -> do
    schema <- obj J..: "schema"
    flip (J.withText "schema") schema $ \t ->
      let schemaDoc = J.fromJSON $ J.String t
      in
      case schemaDoc of
        J.Error err -> fail err
        J.Success a -> return $ RemoteSchemaPermissionDefinition a t

instance J.ToJSON RemoteSchemaPermissionDefinition where
  toJSON (RemoteSchemaPermissionDefinition _ rawSchema) =
    J.object $ [ "schema" J..= J.String rawSchema ]

data AddRemoteSchemaPermissions
  = AddRemoteSchemaPermissions
  { _arspRemoteSchema :: !RemoteSchemaName
  , _arspRole         :: !RoleName
  , _arspDefinition   :: !RemoteSchemaPermissionDefinition
  , _arspComment      :: !(Maybe Text)
  } deriving (Show, Eq, Lift, Generic)
instance NFData AddRemoteSchemaPermissions
instance Cacheable AddRemoteSchemaPermissions
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''AddRemoteSchemaPermissions)

data DropRemoteSchemaPermissions
  = DropRemoteSchemaPermissions
  { _drspRemoteSchema :: !RemoteSchemaName
  , _drspRole         :: !RoleName
  } deriving (Show, Eq, Lift, Generic)
instance NFData DropRemoteSchemaPermissions
instance Cacheable DropRemoteSchemaPermissions
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''DropRemoteSchemaPermissions)

data PartitionedTypeDefinitions a
  = PartitionedTypeDefinitions
  { _ptdScalars      :: ![G.ScalarTypeDefinition]
  , _ptdObjects      :: ![G.ObjectTypeDefinition a]
  , _ptdInterfaces   :: ![G.InterfaceTypeDefinition () a]
  , _ptdUnions       :: ![G.UnionTypeDefinition]
  , _ptdEnums        :: ![G.EnumTypeDefinition]
  , _ptdInputObjects :: ![G.InputObjectTypeDefinition a]
  , _ptdSchemaDef    :: ![G.SchemaDefinition]
  } deriving (Show, Eq)

data RemoteSchemaPresetArgument
  = StaticPresetArgument !G.GType !(G.Value Void)
  | SessionPresetArgument !G.GType !SessionVariable
  -- ^ name of the argument where the session variable is supposed to
  -- be substituted -- session variable name -- value where the session
  -- variable is to be inserted.
  deriving (Show, Eq, Ord, Generic)
instance Hashable RemoteSchemaPresetArgument
instance Cacheable RemoteSchemaPresetArgument

data RemoteSchemaInputValueDefinition
  = RemoteSchemaInputValueDefinition
  { _rsitdDefn      :: !G.InputValueDefinition
  , _rsitdPresetArg :: !(Maybe (G.Value Variable))
  } deriving (Show, Eq, Generic, Ord)
instance Hashable RemoteSchemaInputValueDefinition
instance Cacheable RemoteSchemaInputValueDefinition

newtype RemoteSchemaIntrospection
  = RemoteSchemaIntrospection [(G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)]
  deriving (Show, Eq, Generic, Hashable, Cacheable, Ord)
