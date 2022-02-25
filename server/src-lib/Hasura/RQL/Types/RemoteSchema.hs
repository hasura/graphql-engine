module Hasura.RQL.Types.RemoteSchema
  ( AddRemoteSchemaPermission (..),
    AddRemoteSchemaQuery (..),
    AliasMapping,
    DropRemoteSchemaPermissions (..),
    RemoteFieldCustomization (..),
    RemoteSchemaCustomization (..),
    RemoteSchemaCustomizer (..),
    RemoteSchemaDef (..),
    RemoteSchemaInfo (..),
    RemoteSchemaInputValueDefinition (..),
    RemoteSchemaIntrospection (..),
    RemoteSchemaName (..),
    RemoteSchemaNameQuery (..),
    RemoteSchemaPermissionDefinition (..),
    RemoteSchemaPermsCtx (..),
    RemoteSchemaVariable (..),
    RemoteTypeCustomization (..),
    SessionArgumentPresetInfo (..),
    UrlFromEnv,
    ValidatedRemoteSchemaDef (..),
    applyAliasMapping,
    customizeTypeNameString,
    getUrlFromEnv,
    hasTypeOrFieldCustomizations,
    lookupEnum,
    lookupInputObject,
    lookupInterface,
    lookupObject,
    lookupScalar,
    lookupType,
    lookupUnion,
    modifyFieldByName,
    remoteSchemaCustomizeFieldName,
    getTypeName,
    remoteSchemaCustomizeTypeName,
    singletonAliasMapping,
    validateRemoteSchemaCustomization,
    validateRemoteSchemaDef,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as Q
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Schema
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Headers (HeaderConf (..))
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.SourceCustomization
import Hasura.Session
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Network.URI.Extended qualified as N
import Text.Builder qualified as TB

type UrlFromEnv = Text

-- | Remote schema identifier.
--
-- NOTE: no validation on the character set is done here; it's likely there is
-- a bug (FIXME) where this interacts with remote relationships and some name
-- mangling needs to happen.
newtype RemoteSchemaName = RemoteSchemaName
  {unRemoteSchemaName :: NonEmptyText}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      J.ToJSON,
      J.ToJSONKey,
      J.FromJSON,
      Q.ToPrepArg,
      Q.FromCol,
      ToTxt,
      NFData,
      Generic,
      Cacheable
    )

-- NOTE: Prefix and suffix use 'G.Name' so that we can '<>' to form a new valid
-- by-construction 'G.Name'.
data RemoteTypeCustomization = RemoteTypeCustomization
  { _rtcPrefix :: !(Maybe G.Name),
    _rtcSuffix :: !(Maybe G.Name),
    _rtcMapping :: !(HashMap G.Name G.Name)
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteTypeCustomization

instance Cacheable RemoteTypeCustomization

instance Hashable RemoteTypeCustomization

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''RemoteTypeCustomization)

instance J.FromJSON RemoteTypeCustomization where
  parseJSON = J.withObject "RemoteTypeCustomization" $ \o ->
    RemoteTypeCustomization
      <$> o J..:? "prefix"
      <*> o J..:? "suffix"
      <*> o J..:? "mapping" J..!= mempty

data RemoteFieldCustomization = RemoteFieldCustomization
  { _rfcParentType :: !G.Name,
    _rfcPrefix :: !(Maybe G.Name),
    _rfcSuffix :: !(Maybe G.Name),
    _rfcMapping :: !(HashMap G.Name G.Name)
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteFieldCustomization

instance Cacheable RemoteFieldCustomization

instance Hashable RemoteFieldCustomization

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''RemoteFieldCustomization)

instance J.FromJSON RemoteFieldCustomization where
  parseJSON = J.withObject "RemoteFieldCustomization" $ \o ->
    RemoteFieldCustomization
      <$> o J..: "parent_type"
      <*> o J..:? "prefix"
      <*> o J..:? "suffix"
      <*> o J..:? "mapping" J..!= mempty

data RemoteSchemaCustomization = RemoteSchemaCustomization
  { _rscRootFieldsNamespace :: !(Maybe G.Name),
    _rscTypeNames :: !(Maybe RemoteTypeCustomization),
    _rscFieldNames :: !(Maybe [RemoteFieldCustomization])
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaCustomization

instance Cacheable RemoteSchemaCustomization

instance Hashable RemoteSchemaCustomization

$(J.deriveJSON hasuraJSON {J.omitNothingFields = True} ''RemoteSchemaCustomization)

-- | 'RemoteSchemaDef' after validation and baking-in of defaults in 'validateRemoteSchemaDef'.
data ValidatedRemoteSchemaDef = ValidatedRemoteSchemaDef
  { _vrsdUrl :: !N.URI,
    _vrsdHeaders :: ![HeaderConf],
    _vrsdFwdClientHeaders :: !Bool,
    _vrsdTimeoutSeconds :: !Int,
    -- | See '_rsdCustomization'.
    _vrsdCustomization :: !(Maybe RemoteSchemaCustomization)
  }
  deriving (Show, Eq, Generic)

instance NFData ValidatedRemoteSchemaDef

instance Cacheable ValidatedRemoteSchemaDef

instance Hashable ValidatedRemoteSchemaDef

$(J.deriveJSON hasuraJSON ''ValidatedRemoteSchemaDef)

data RemoteSchemaCustomizer = RemoteSchemaCustomizer
  { _rscNamespaceFieldName :: !(Maybe G.Name),
    -- | type name -> type name
    _rscCustomizeTypeName :: !(HashMap G.Name G.Name),
    -- | type name -> field name -> field name
    _rscCustomizeFieldName :: !(HashMap G.Name (HashMap G.Name G.Name))
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaCustomizer

instance Cacheable RemoteSchemaCustomizer

instance Hashable RemoteSchemaCustomizer

$(J.deriveJSON hasuraJSON ''RemoteSchemaCustomizer)

remoteSchemaCustomizeTypeName :: RemoteSchemaCustomizer -> MkTypename
remoteSchemaCustomizeTypeName RemoteSchemaCustomizer {..} = MkTypename $ \typeName ->
  Map.lookupDefault typeName typeName _rscCustomizeTypeName

remoteSchemaCustomizeFieldName :: RemoteSchemaCustomizer -> CustomizeRemoteFieldName
remoteSchemaCustomizeFieldName RemoteSchemaCustomizer {..} = CustomizeRemoteFieldName $ \typeName fieldName ->
  Map.lookup typeName _rscCustomizeFieldName >>= Map.lookup fieldName & fromMaybe fieldName

hasTypeOrFieldCustomizations :: RemoteSchemaCustomizer -> Bool
hasTypeOrFieldCustomizations RemoteSchemaCustomizer {..} =
  not $ Map.null _rscCustomizeTypeName && Map.null _rscCustomizeFieldName

-- | 'RemoteSchemaDef' after the RemoteSchemaCustomizer has been generated
-- by fetchRemoteSchema
data RemoteSchemaInfo = RemoteSchemaInfo
  { rsDef :: !ValidatedRemoteSchemaDef,
    rsCustomizer :: !RemoteSchemaCustomizer
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaInfo

instance Cacheable RemoteSchemaInfo

instance Hashable RemoteSchemaInfo

$(J.deriveJSON hasuraJSON ''RemoteSchemaInfo)

-- | Unvalidated remote schema config, from the user's API request
data RemoteSchemaDef = RemoteSchemaDef
  { _rsdUrl :: !(Maybe InputWebhook),
    _rsdUrlFromEnv :: !(Maybe UrlFromEnv),
    _rsdHeaders :: !(Maybe [HeaderConf]),
    _rsdForwardClientHeaders :: !Bool,
    _rsdTimeoutSeconds :: !(Maybe Int),
    _rsdCustomization :: !(Maybe RemoteSchemaCustomization)
    -- NOTE: In the future we might extend this API to support a small DSL of
    -- name transformations; this might live at a different layer, and be part of
    -- the schema customization story.
    --
    -- See: https://github.com/hasura/graphql-engine-mono/issues/144
    -- TODO we probably want to move this into a sub-field "transformations"?
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaDef

instance Cacheable RemoteSchemaDef

$(J.deriveToJSON hasuraJSON {J.omitNothingFields = True} ''RemoteSchemaDef)

instance J.FromJSON RemoteSchemaDef where
  parseJSON = J.withObject "Object" $ \o ->
    RemoteSchemaDef
      <$> o J..:? "url"
      <*> o J..:? "url_from_env"
      <*> o J..:? "headers"
      <*> o J..:? "forward_client_headers" J..!= False
      <*> o J..:? "timeout_seconds"
      <*> o J..:? "customization"

-- | The payload for 'add_remote_schema', and a component of 'Metadata'.
data AddRemoteSchemaQuery = AddRemoteSchemaQuery
  { -- | An internal identifier for this remote schema.
    _arsqName :: !RemoteSchemaName,
    _arsqDefinition :: !RemoteSchemaDef,
    -- | An opaque description or comment. We might display this in the UI, for instance.
    _arsqComment :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance NFData AddRemoteSchemaQuery

instance Cacheable AddRemoteSchemaQuery

$(J.deriveJSON hasuraJSON ''AddRemoteSchemaQuery)

newtype RemoteSchemaNameQuery = RemoteSchemaNameQuery
  { _rsnqName :: RemoteSchemaName
  }
  deriving (Show, Eq)

$(J.deriveJSON hasuraJSON ''RemoteSchemaNameQuery)

getUrlFromEnv :: (MonadIO m, MonadError QErr m) => Env.Environment -> Text -> m N.URI
getUrlFromEnv env urlFromEnv = do
  let mEnv = Env.lookupEnv env $ T.unpack urlFromEnv
  uri <- onNothing mEnv (throw400 InvalidParams $ envNotFoundMsg urlFromEnv)
  onNothing (N.parseURI uri) (throw400 InvalidParams $ invalidUri uri)
  where
    invalidUri x = "not a valid URI: " <> T.pack x
    envNotFoundMsg e = "environment variable '" <> e <> "' not set"

validateRemoteSchemaCustomization ::
  (MonadError QErr m) =>
  Maybe RemoteSchemaCustomization ->
  m ()
validateRemoteSchemaCustomization Nothing = pure ()
validateRemoteSchemaCustomization (Just RemoteSchemaCustomization {..}) =
  for_ _rscFieldNames $ \fieldCustomizations ->
    for_ fieldCustomizations $ \RemoteFieldCustomization {..} ->
      for_ (Map.keys _rfcMapping) $ \fieldName ->
        when (isReservedName fieldName) $
          throw400 InvalidParams $ "attempt to customize reserved field name " <>> fieldName
  where
    isReservedName = ("__" `T.isPrefixOf`) . G.unName

validateRemoteSchemaDef ::
  (MonadError QErr m, MonadIO m) =>
  Env.Environment ->
  RemoteSchemaDef ->
  m ValidatedRemoteSchemaDef
validateRemoteSchemaDef env (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs mTimeout customization) = do
  validateRemoteSchemaCustomization customization
  case (mUrl, mUrlEnv) of
    (Just url, Nothing) -> do
      resolvedWebhookTxt <- unResolvedWebhook <$> resolveWebhook env url
      case N.parseURI $ T.unpack resolvedWebhookTxt of
        Nothing -> throw400 InvalidParams $ "not a valid URI: " <> resolvedWebhookTxt
        Just uri -> return $ ValidatedRemoteSchemaDef uri hdrs fwdHdrs timeout customization
    (Nothing, Just urlEnv) -> do
      url <- getUrlFromEnv env urlEnv
      return $ ValidatedRemoteSchemaDef url hdrs fwdHdrs timeout customization
    (Nothing, Nothing) ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    (Just _, Just _) ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be present"
  where
    hdrs = fromMaybe [] hdrC

    timeout = fromMaybe 60 mTimeout

newtype RemoteSchemaPermissionDefinition = RemoteSchemaPermissionDefinition
  { _rspdSchema :: G.SchemaDocument
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaPermissionDefinition

instance Cacheable RemoteSchemaPermissionDefinition

instance Hashable RemoteSchemaPermissionDefinition

instance J.FromJSON RemoteSchemaPermissionDefinition where
  parseJSON = J.withObject "RemoteSchemaPermissionDefinition" $ \obj -> do
    fmap RemoteSchemaPermissionDefinition $ obj J..: "schema"

instance J.ToJSON RemoteSchemaPermissionDefinition where
  toJSON (RemoteSchemaPermissionDefinition schema) =
    J.object $ ["schema" J..= J.String (TB.run . G.schemaDocument $ schema)]

data AddRemoteSchemaPermission = AddRemoteSchemaPermission
  { _arspRemoteSchema :: !RemoteSchemaName,
    _arspRole :: !RoleName,
    _arspDefinition :: !RemoteSchemaPermissionDefinition,
    _arspComment :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance NFData AddRemoteSchemaPermission

instance Cacheable AddRemoteSchemaPermission

$(J.deriveJSON hasuraJSON ''AddRemoteSchemaPermission)

data DropRemoteSchemaPermissions = DropRemoteSchemaPermissions
  { _drspRemoteSchema :: !RemoteSchemaName,
    _drspRole :: !RoleName
  }
  deriving (Show, Eq, Generic)

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

-- | Details required to resolve a "session variable preset" variable.
--
-- See Notes [Remote Schema Argument Presets] and [Remote Schema Permissions
-- Architecture] for additional information.
data RemoteSchemaVariable
  = SessionPresetVariable !SessionVariable !G.Name !SessionArgumentPresetInfo
  | QueryVariable !Variable
  | RemoteJSONValue !G.GType !J.Value
  deriving (Show, Eq, Generic, Ord)

instance Hashable RemoteSchemaVariable

instance Cacheable RemoteSchemaVariable

-- | Extends 'G.InputValueDefinition' with an optional preset argument.
--
-- See Note [Remote Schema Argument Presets] for additional information.
data RemoteSchemaInputValueDefinition = RemoteSchemaInputValueDefinition
  { _rsitdDefinition :: !G.InputValueDefinition,
    _rsitdPresetArgument :: !(Maybe (G.Value RemoteSchemaVariable))
  }
  deriving (Show, Eq, Generic, Ord)

instance Hashable RemoteSchemaInputValueDefinition

instance Cacheable RemoteSchemaInputValueDefinition

newtype RemoteSchemaIntrospection
  = RemoteSchemaIntrospection (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
  deriving (Show, Eq, Generic, Hashable, Cacheable, Ord)

data RemoteSchemaPermsCtx
  = RemoteSchemaPermsEnabled
  | RemoteSchemaPermsDisabled
  deriving (Show, Eq)

instance J.FromJSON RemoteSchemaPermsCtx where
  parseJSON =
    J.withBool "RemoteSchemaPermsCtx" $
      pure . bool RemoteSchemaPermsDisabled RemoteSchemaPermsEnabled

instance J.ToJSON RemoteSchemaPermsCtx where
  toJSON = \case
    RemoteSchemaPermsEnabled -> J.Bool True
    RemoteSchemaPermsDisabled -> J.Bool False

-- | Extracts the name of a given type from its definition.
-- TODO: move this to Language.GraphQL.Draft.Syntax.
getTypeName :: G.TypeDefinition possibleTypes inputType -> G.Name
getTypeName = \case
  G.TypeDefinitionScalar t -> G._stdName t
  G.TypeDefinitionObject t -> G._otdName t
  G.TypeDefinitionInterface t -> G._itdName t
  G.TypeDefinitionUnion t -> G._utdName t
  G.TypeDefinitionEnum t -> G._etdName t
  G.TypeDefinitionInputObject t -> G._iotdName t

lookupType ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
lookupType (RemoteSchemaIntrospection types) name = Map.lookup name types

lookupObject ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
lookupObject introspection name =
  lookupType introspection name >>= \case
    G.TypeDefinitionObject t | G._otdName t == name -> Just t
    -- if this happens, it means the schema is inconsistent: we expected to
    -- find an object with that name, but instead found something that wasn't
    -- an object; we might want to indicate this with a proper failure, so we
    -- can show better diagnostics to the user?
    -- This also applies to all following functions.
    -- See: https://github.com/hasura/graphql-engine-mono/issues/2991
    _ -> Nothing

lookupInterface ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe (G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
lookupInterface introspection name =
  lookupType introspection name >>= \case
    G.TypeDefinitionInterface t | G._itdName t == name -> Just t
    _ -> Nothing

lookupScalar ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe G.ScalarTypeDefinition
lookupScalar introspection name =
  lookupType introspection name >>= \case
    G.TypeDefinitionScalar t | G._stdName t == name -> Just t
    _ -> Nothing

lookupUnion ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe G.UnionTypeDefinition
lookupUnion introspection name =
  lookupType introspection name >>= \case
    G.TypeDefinitionUnion t | G._utdName t == name -> Just t
    _ -> Nothing

lookupEnum ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe G.EnumTypeDefinition
lookupEnum introspection name =
  lookupType introspection name >>= \case
    G.TypeDefinitionEnum t | G._etdName t == name -> Just t
    _ -> Nothing

lookupInputObject ::
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe (G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition)
lookupInputObject introspection name =
  lookupType introspection name >>= \case
    G.TypeDefinitionInputObject t | G._iotdName t == name -> Just t
    _ -> Nothing
