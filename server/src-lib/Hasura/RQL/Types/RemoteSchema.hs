module Hasura.RQL.Types.RemoteSchema where

import           Hasura.Prelude

import qualified Data.Aeson                     as J
import qualified Data.Aeson.Ordered             as JO
import qualified Data.Aeson.TH                  as J
import qualified Data.Environment               as Env
import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import qualified Database.PG.Query              as Q
import qualified Language.GraphQL.Draft.Printer as G
import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Network.URI.Extended           as N
import qualified Text.Builder                   as TB

import           Control.Lens.TH                (makeLenses)
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Data.Monoid                    (Endo (..))
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
           , Generic, Cacheable
           )

  -- NOTE: Prefix and suffix use 'G.Name' so that we can '<>' to form a new valid
  -- by-construction 'G.Name'.
data RemoteTypeCustomization
  = RemoteTypeCustomization
  { _rtcPrefix  :: !(Maybe G.Name)
  , _rtcSuffix  :: !(Maybe G.Name)
  , _rtcMapping :: !(HashMap G.Name G.Name)
  } deriving (Show, Eq, Generic)
instance NFData RemoteTypeCustomization
instance Cacheable RemoteTypeCustomization
instance Hashable RemoteTypeCustomization
$(J.deriveToJSON hasuraJSON{J.omitNothingFields=True} ''RemoteTypeCustomization)

instance J.FromJSON RemoteTypeCustomization where
  parseJSON = J.withObject "RemoteTypeCustomization" $ \o ->
    RemoteTypeCustomization
    <$> o J..:? "prefix"
    <*> o J..:? "suffix"
    <*> o J..:? "mapping" J..!= mempty

data RemoteFieldCustomization
  = RemoteFieldCustomization
  { _rfcParentType :: !G.Name
  , _rfcPrefix     :: !(Maybe G.Name)
  , _rfcSuffix     :: !(Maybe G.Name)
  , _rfcMapping    :: !(HashMap G.Name G.Name)
  } deriving (Show, Eq, Generic)
instance NFData RemoteFieldCustomization
instance Cacheable RemoteFieldCustomization
instance Hashable RemoteFieldCustomization
$(J.deriveToJSON hasuraJSON{J.omitNothingFields=True} ''RemoteFieldCustomization)

instance J.FromJSON RemoteFieldCustomization where
  parseJSON = J.withObject "RemoteFieldCustomization" $ \o ->
    RemoteFieldCustomization
    <$> o J..: "parent_type"
    <*> o J..:? "prefix"
    <*> o J..:? "suffix"
    <*> o J..:? "mapping" J..!= mempty

data RemoteSchemaCustomization
  = RemoteSchemaCustomization
  { _rscRootFieldsNamespace :: !(Maybe G.Name)
  , _rscTypeNames           :: !(Maybe RemoteTypeCustomization)
  , _rscFieldNames          :: !(Maybe [RemoteFieldCustomization])
  } deriving (Show, Eq, Generic)
instance NFData RemoteSchemaCustomization
instance Cacheable RemoteSchemaCustomization
instance Hashable RemoteSchemaCustomization
$(J.deriveJSON hasuraJSON{J.omitNothingFields=True} ''RemoteSchemaCustomization)

-- | 'RemoteSchemaDef' after validation and baking-in of defaults in 'validateRemoteSchemaDef'.
data ValidatedRemoteSchemaDef
  = ValidatedRemoteSchemaDef
  { _vrsdUrl              :: !N.URI
  , _vrsdHeaders          :: ![HeaderConf]
  , _vrsdFwdClientHeaders :: !Bool
  , _vrsdTimeoutSeconds   :: !Int
  , _vrsdCustomization    :: !(Maybe RemoteSchemaCustomization)
  -- ^ See '_rsdCustomization'.
  } deriving (Show, Eq, Generic)
instance NFData ValidatedRemoteSchemaDef
instance Cacheable ValidatedRemoteSchemaDef
instance Hashable ValidatedRemoteSchemaDef

$(J.deriveJSON hasuraJSON ''ValidatedRemoteSchemaDef)

data RemoteSchemaCustomizer
  = RemoteSchemaCustomizer
  { _rscNamespaceFieldName   :: !(Maybe G.Name)
  , _rscCustomizeTypeName    :: !(HashMap G.Name G.Name)                  -- ^ type name -> type name
  , _rscCustomizeFieldName   :: !(HashMap G.Name (HashMap G.Name G.Name)) -- ^ type name -> field name -> field name
  , _rscDecustomizeTypeName  :: !(HashMap G.Name G.Name)                  -- ^ type name -> type name
  , _rscDecustomizeFieldName :: !(HashMap G.Name (HashMap G.Name G.Name)) -- ^ type name -> field name -> field name
  } deriving (Show, Eq, Generic)
instance NFData RemoteSchemaCustomizer
instance Cacheable RemoteSchemaCustomizer
instance Hashable RemoteSchemaCustomizer

$(J.deriveJSON hasuraJSON ''RemoteSchemaCustomizer)

remoteSchemaCustomizeTypeName :: RemoteSchemaCustomizer -> G.Name -> G.Name
remoteSchemaCustomizeTypeName RemoteSchemaCustomizer{..} typeName =
  Map.lookupDefault typeName typeName _rscCustomizeTypeName

remoteSchemaCustomizeFieldName :: RemoteSchemaCustomizer -> G.Name -> G.Name -> G.Name
remoteSchemaCustomizeFieldName RemoteSchemaCustomizer{..} typeName fieldName =
  Map.lookup typeName _rscCustomizeFieldName >>= Map.lookup fieldName & fromMaybe fieldName

remoteSchemaDecustomizeTypeName :: RemoteSchemaCustomizer -> G.Name -> G.Name
remoteSchemaDecustomizeTypeName RemoteSchemaCustomizer{..} typeName =
  Map.lookupDefault typeName typeName _rscDecustomizeTypeName

remoteSchemaDecustomizeFieldName :: RemoteSchemaCustomizer -> G.Name -> G.Name -> G.Name
remoteSchemaDecustomizeFieldName RemoteSchemaCustomizer{..} typeName fieldName =
  Map.lookup typeName _rscDecustomizeFieldName >>= Map.lookup fieldName & fromMaybe fieldName

hasTypeOrFieldCustomizations :: RemoteSchemaCustomizer -> Bool
hasTypeOrFieldCustomizations RemoteSchemaCustomizer{..} =
  not $ Map.null _rscCustomizeTypeName && Map.null _rscCustomizeFieldName

-- | 'RemoteSchemaDef' after the RemoteSchemaCustomizer has been generated
-- by fetchRemoteSchema
data RemoteSchemaInfo
  = RemoteSchemaInfo
  { rsDef        :: !ValidatedRemoteSchemaDef
  , rsCustomizer :: !RemoteSchemaCustomizer
  } deriving (Show, Eq, Generic)
instance NFData RemoteSchemaInfo
instance Cacheable RemoteSchemaInfo
instance Hashable RemoteSchemaInfo

$(J.deriveJSON hasuraJSON ''RemoteSchemaInfo)

-- | Unvalidated remote schema config, from the user's API request
data RemoteSchemaDef
  = RemoteSchemaDef
  { _rsdUrl                  :: !(Maybe InputWebhook)
  , _rsdUrlFromEnv           :: !(Maybe UrlFromEnv)
  , _rsdHeaders              :: !(Maybe [HeaderConf])
  , _rsdForwardClientHeaders :: !Bool
  , _rsdTimeoutSeconds       :: !(Maybe Int)
  , _rsdCustomization        :: !(Maybe RemoteSchemaCustomization)
  -- NOTE: In the future we might extend this API to support a small DSL of
  -- name transformations; this might live at a different layer, and be part of
  -- the schema customization story.
  --
  -- See: https://github.com/hasura/graphql-engine-mono/issues/144
  -- TODO we probably want to move this into a sub-field "transformations"?
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
      <*> o J..:? "customization"

-- | The payload for 'add_remote_schema', and a component of 'Metadata'.
data AddRemoteSchemaQuery
  = AddRemoteSchemaQuery
  { _arsqName       :: !RemoteSchemaName
  -- ^ An internal identifier for this remote schema.
  , _arsqDefinition :: !RemoteSchemaDef
  , _arsqComment    :: !(Maybe Text)
  -- ^ An opaque description or comment. We might display this in the UI, for instance.
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

validateRemoteSchemaCustomization
  :: (MonadError QErr m)
  => Maybe RemoteSchemaCustomization
  -> m ()
validateRemoteSchemaCustomization Nothing = pure ()
validateRemoteSchemaCustomization (Just RemoteSchemaCustomization{..}) =
  for_ _rscFieldNames $ \fieldCustomizations ->
    for_ fieldCustomizations $ \RemoteFieldCustomization{..} ->
      for_ (Map.keys _rfcMapping) $ \fieldName ->
        when (isReservedName fieldName) $
          throw400 InvalidParams $ "attempt to customize reserved field name " <>> fieldName
  where
    isReservedName = ("__" `T.isPrefixOf`) . G.unName

validateRemoteSchemaDef
  :: (MonadError QErr m, MonadIO m)
  => Env.Environment
  -> RemoteSchemaDef
  -> m ValidatedRemoteSchemaDef
validateRemoteSchemaDef env (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs mTimeout customization) = do
  validateRemoteSchemaCustomization customization
  case (mUrl, mUrlEnv) of
    (Just url, Nothing)    -> do
      resolvedWebhookTxt <- unResolvedWebhook <$> resolveWebhook env url
      case N.parseURI $ T.unpack resolvedWebhookTxt of
        Nothing  -> throw400 InvalidParams $ "not a valid URI: " <> resolvedWebhookTxt
        Just uri -> return $ ValidatedRemoteSchemaDef uri hdrs fwdHdrs timeout customization
    (Nothing, Just urlEnv) -> do
      url <- getUrlFromEnv env urlEnv
      return $ ValidatedRemoteSchemaDef url hdrs fwdHdrs timeout customization
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

data AddRemoteSchemaPermission
  = AddRemoteSchemaPermission
  { _arspRemoteSchema :: !RemoteSchemaName
  , _arspRole         :: !RoleName
  , _arspDefinition   :: !RemoteSchemaPermissionDefinition
  , _arspComment      :: !(Maybe Text)
  } deriving (Show, Eq, Generic)
instance NFData AddRemoteSchemaPermission
instance Cacheable AddRemoteSchemaPermission
$(J.deriveJSON hasuraJSON ''AddRemoteSchemaPermission)

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

-- | An RemoteRootField could either be a real field on the remote server
-- or represent a virtual namespace that only exists in the Hasura schema.
data RemoteRootField var
  = RRFNamespaceField !(G.SelectionSet G.NoFragments var) -- ^ virtual namespace field
  | RRFRealField !(G.Field G.NoFragments var) -- ^ a real field on the remote server
  deriving (Functor, Foldable, Traversable)

-- | For a real remote field gives a SelectionSet for selecting the field itself.
--   For a virtual field gives the unwrapped SelectionSet for the field.
getRemoteFieldSelectionSet :: RemoteRootField var -> G.SelectionSet G.NoFragments var
getRemoteFieldSelectionSet = \case
    RRFNamespaceField selSet -> selSet
    RRFRealField fld         -> [G.SelectionField fld]

-- | Mapping that can be provided to a RemoteResultCustomizer
-- to map top-level field aliases that were not available at field parse time.
-- E.g. for aliases created in the remote server query for remote joins.
newtype AliasMapping = AliasMapping { unAliasMapping :: Endo G.Name }
  deriving (Semigroup, Monoid)

-- | AliasMapping that maps a single field name to an alias
singletonAliasMapping :: G.Name -> G.Name -> AliasMapping
singletonAliasMapping fieldName alias = AliasMapping $ Endo $ \fieldName' ->
  if fieldName == fieldName' then alias else fieldName'

-- | Function to modify JSON values returned from the remote server
-- e.g. to map values of __typename fields to customized type names.
-- The customizer uses Maybe to allow short-circuiting subtrees
-- where no customizations are needed.
newtype RemoteResultCustomizer =
  RemoteResultCustomizer { unRemoteResultCustomizer :: Maybe (AliasMapping -> Endo JO.Value) }
  deriving (Semigroup, Monoid)

-- | Apply a RemoteResultCustomizer to a JSON value
applyRemoteResultCustomizer :: RemoteResultCustomizer -> JO.Value -> JO.Value
applyRemoteResultCustomizer = maybe id (appEndo . ($ mempty)) . unRemoteResultCustomizer

-- | Apply an AliasMapping to a RemoteResultCustomizer.
applyAliasMapping :: AliasMapping -> RemoteResultCustomizer -> RemoteResultCustomizer
applyAliasMapping aliasMapping (RemoteResultCustomizer m) = RemoteResultCustomizer $
  m <&> \g aliasMapping' -> g $ aliasMapping' <> aliasMapping

-- | Take a RemoteResultCustomizer for a JSON subtree, and a fieldName,
-- and produce a RemoteResultCustomizer for a parent object or array of objects
-- that applies the subtree customizer to the subtree at the given fieldName.
modifyFieldByName :: G.Name -> RemoteResultCustomizer -> RemoteResultCustomizer
modifyFieldByName fieldName (RemoteResultCustomizer m) = RemoteResultCustomizer $
  m <&> \g aliasMapping -> Endo $
    let Endo f = g mempty -- AliasMapping is only applied to the top level so use mempty for nested customizers
        modifyFieldByName' = \case
          JO.Object o -> JO.Object $ JO.adjust f (G.unName $ (appEndo $ unAliasMapping aliasMapping) fieldName) o
          JO.Array a  -> JO.Array $ modifyFieldByName' <$> a
          v           -> v
    in modifyFieldByName'

-- | Create a RemoteResultCustomizer that applies the typeNameMap
-- to a JSON string value, e.g. for use in customizing a __typename field value.
customizeTypeNameString :: HashMap G.Name G.Name -> RemoteResultCustomizer
customizeTypeNameString typeNameMap =
  if Map.null typeNameMap
    then mempty
    else RemoteResultCustomizer $ Just $ const $ Endo $ \case
      JO.String t -> JO.String $ G.unName $ customizeTypeName $ G.unsafeMkName t
      v           -> v
  where
    customizeTypeName :: G.Name -> G.Name
    customizeTypeName typeName = Map.lookupDefault typeName typeName typeNameMap

data RemoteFieldG f var
  = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RemoteSchemaInfo
  , _rfResultCustomizer :: !RemoteResultCustomizer
  , _rfField            :: !(f var)
  } deriving (Functor, Foldable, Traversable)

$(makeLenses ''RemoteFieldG)

type RawRemoteField = RemoteFieldG (G.Field G.NoFragments) RemoteSchemaVariable

type RemoteField = RemoteFieldG RemoteRootField RemoteSchemaVariable

realRemoteField :: RawRemoteField -> RemoteField
realRemoteField RemoteFieldG{..} = RemoteFieldG{_rfField = RRFRealField _rfField, ..}

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
