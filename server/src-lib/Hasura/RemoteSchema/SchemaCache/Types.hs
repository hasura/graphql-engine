{-# LANGUAGE TemplateHaskell #-}

module Hasura.RemoteSchema.SchemaCache.Types
  ( RemoteSchemaRelationshipsG,
    IntrospectionResult (..),
    RemoteSchemaCtxG (..),
    PartiallyResolvedRemoteRelationship (..),
    PartiallyResolvedRemoteSchemaCtxG,
    rscName,
    rscInfo,
    rscIntroOriginal,
    rscRawIntrospectionResult,
    rscPermissions,
    rscRemoteRelationships,
    RemoteSchemaCustomizer (..),
    RemoteSchemaInfo (..),
    ValidatedRemoteSchemaDef (..),
    hasTypeOrFieldCustomizations,
    identityCustomizer,
    remoteSchemaCustomizeFieldName,
    remoteSchemaCustomizeTypeName,
    validateRemoteSchemaCustomization,
    validateRemoteSchemaDef,
    CustomizeRemoteFieldName (..),
    withRemoteFieldNameCustomization,
    RemoteSchemaInputValueDefinition (..),
    RemoteSchemaIntrospection (..),
    RemoteSchemaVariable (..),
    SessionArgumentPresetInfo (..),
    lookupEnum,
    lookupInputObject,
    lookupInterface,
    lookupObject,
    lookupScalar,
    lookupType,
    lookupUnion,
    getTypeName,
    FieldCall (..),
    RemoteArguments (..),
    RemoteFields (..),
    RemoteSchemaFieldInfo (..),
    graphQLValueToJSON,
    LHSIdentifier (..),
    remoteSchemaToLHSIdentifier,
    lhsIdentifierToGraphQLName,
  )
where

import Control.Lens
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.Char qualified as C
import Data.Environment qualified as Env
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Extended
import Data.URL.Template (printTemplate)
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Variable
import Hasura.GraphQL.Schema.Typename
import Hasura.Prelude
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Headers (HeaderConf (..))
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RemoteSchema.Metadata
import Hasura.Session (SessionVariable)
import Language.GraphQL.Draft.Syntax qualified as G
import Network.URI.Extended qualified as N
import Witherable (Filterable (..))

type RemoteSchemaRelationshipsG remoteFieldInfo =
  InsOrdHashMap G.Name (InsOrdHashMap RelName remoteFieldInfo)

data IntrospectionResult = IntrospectionResult
  { irDoc :: RemoteSchemaIntrospection,
    irQueryRoot :: G.Name,
    irMutationRoot :: Maybe G.Name,
    irSubscriptionRoot :: Maybe G.Name
  }
  deriving (Show, Eq, Generic)

-- | The resolved information of a remote schema. It is parameterized by
-- `remoteFieldInfo` so as to work on an arbitrary 'remote relationship'
-- TODO: Get rid of this 'G' suffix using pattern synonyms or qualified
-- usage
data RemoteSchemaCtxG remoteFieldInfo = RemoteSchemaCtx
  { _rscName :: RemoteSchemaName,
    -- | Original remote schema without customizations
    _rscIntroOriginal :: IntrospectionResult,
    _rscInfo :: RemoteSchemaInfo,
    -- | The raw response from the introspection query against the remote server.
    -- We store this so we can efficiently service 'introspect_remote_schema'.
    _rscRawIntrospectionResult :: BL.ByteString,
    _rscPermissions :: HashMap.HashMap RoleName IntrospectionResult,
    _rscRemoteRelationships :: RemoteSchemaRelationshipsG remoteFieldInfo
  }
  deriving (Eq, Functor, Foldable, Traversable)

instance Filterable RemoteSchemaCtxG where
  filter f RemoteSchemaCtx {..} =
    RemoteSchemaCtx
      { _rscRemoteRelationships = fmap (Witherable.filter f) _rscRemoteRelationships,
        ..
      }
  mapMaybe f RemoteSchemaCtx {..} =
    RemoteSchemaCtx
      { _rscRemoteRelationships = fmap (mapMaybe f) _rscRemoteRelationships,
        ..
      }

-- | Resolved information of a remote relationship with the local information
-- that we have. Currently this is only the typename on which the relationship
-- is defined. TODO: also add the available join fields on the type
data PartiallyResolvedRemoteRelationship remoteRelationshipDefinition = PartiallyResolvedRemoteRelationship
  { _prrrTypeName :: G.Name,
    _prrrDefinition :: RemoteRelationshipG remoteRelationshipDefinition
  }
  deriving (Eq, Generic)

-- | We can't go from RemoteSchemaMetadata to RemoteSchemaCtx in a single phase
-- because we don't have information to resolve remote relationships. So we
-- annotate remote relationships with as much information as we know about them
-- which would be further resolved in a later stage.
type PartiallyResolvedRemoteSchemaCtxG remoteRelationshipDefinition =
  RemoteSchemaCtxG (PartiallyResolvedRemoteRelationship remoteRelationshipDefinition)

-- | 'RemoteSchemaDef' after validation and baking-in of defaults in 'validateRemoteSchemaDef'.
data ValidatedRemoteSchemaDef = ValidatedRemoteSchemaDef
  { _vrsdName :: RemoteSchemaName,
    _vrsdUrl :: EnvRecord N.URI,
    _vrsdHeaders :: [HeaderConf],
    _vrsdFwdClientHeaders :: Bool,
    _vrsdTimeoutSeconds :: Int,
    -- | See '_rsdCustomization'.
    _vrsdCustomization :: Maybe RemoteSchemaCustomization
  }
  deriving (Show, Eq, Generic)

instance NFData ValidatedRemoteSchemaDef

instance Hashable ValidatedRemoteSchemaDef

data RemoteSchemaCustomizer = RemoteSchemaCustomizer
  { _rscNamespaceFieldName :: Maybe G.Name,
    -- | type name -> type name
    _rscCustomizeTypeName :: HashMap G.Name G.Name,
    -- | type name -> field name -> field name
    _rscCustomizeFieldName :: HashMap G.Name (HashMap G.Name G.Name)
  }
  deriving (Show, Eq, Generic)

identityCustomizer :: RemoteSchemaCustomizer
identityCustomizer = RemoteSchemaCustomizer Nothing mempty mempty

instance NFData RemoteSchemaCustomizer

instance Hashable RemoteSchemaCustomizer

remoteSchemaCustomizeTypeName :: RemoteSchemaCustomizer -> MkTypename
remoteSchemaCustomizeTypeName RemoteSchemaCustomizer {..} = MkTypename $ \typeName ->
  HashMap.lookupDefault typeName typeName _rscCustomizeTypeName

newtype CustomizeRemoteFieldName = CustomizeRemoteFieldName
  { runCustomizeRemoteFieldName :: G.Name -> G.Name -> G.Name
  }
  deriving (Semigroup, Monoid) via (G.Name -> Endo G.Name)

withRemoteFieldNameCustomization :: forall m r a. (MonadReader r m, Has CustomizeRemoteFieldName r) => CustomizeRemoteFieldName -> m a -> m a
withRemoteFieldNameCustomization = local . set hasLens

remoteSchemaCustomizeFieldName :: RemoteSchemaCustomizer -> CustomizeRemoteFieldName
remoteSchemaCustomizeFieldName RemoteSchemaCustomizer {..} = CustomizeRemoteFieldName $ \typeName fieldName ->
  HashMap.lookup typeName _rscCustomizeFieldName >>= HashMap.lookup fieldName & fromMaybe fieldName

hasTypeOrFieldCustomizations :: RemoteSchemaCustomizer -> Bool
hasTypeOrFieldCustomizations RemoteSchemaCustomizer {..} =
  not $ HashMap.null _rscCustomizeTypeName && HashMap.null _rscCustomizeFieldName

-- | 'RemoteSchemaDef' after the RemoteSchemaCustomizer has been generated
-- by fetchRemoteSchema
data RemoteSchemaInfo = RemoteSchemaInfo
  { rsDef :: ValidatedRemoteSchemaDef,
    rsCustomizer :: RemoteSchemaCustomizer
  }
  deriving (Show, Eq, Generic)

instance NFData RemoteSchemaInfo

instance Hashable RemoteSchemaInfo

validateRemoteSchemaCustomization ::
  (MonadError QErr m) =>
  Maybe RemoteSchemaCustomization ->
  m ()
validateRemoteSchemaCustomization Nothing = pure ()
validateRemoteSchemaCustomization (Just RemoteSchemaCustomization {..}) =
  for_ _rscFieldNames $ \fieldCustomizations ->
    for_ fieldCustomizations $ \RemoteFieldCustomization {..} ->
      for_ (HashMap.keys _rfcMapping) $ \fieldName ->
        when (isReservedName fieldName)
          $ throw400 InvalidParams
          $ "attempt to customize reserved field name "
          <>> fieldName
  where
    isReservedName = ("__" `T.isPrefixOf`) . G.unName

validateRemoteSchemaDef ::
  (MonadError QErr m) =>
  RemoteSchemaName ->
  Env.Environment ->
  RemoteSchemaDef ->
  m ValidatedRemoteSchemaDef
validateRemoteSchemaDef name env (RemoteSchemaDef mUrl mUrlEnv hdrC fwdHdrs mTimeout customization) = do
  validateRemoteSchemaCustomization customization
  case (mUrl, mUrlEnv) of
    -- case 1: URL is supplied as a template
    (Just url, Nothing) -> do
      resolvedWebhookTxt <- unResolvedWebhook <$> resolveWebhook env url
      case N.parseURI $ T.unpack resolvedWebhookTxt of
        Nothing -> throw400 InvalidParams $ "not a valid URI generated from the template: " <> getTemplateFromUrl url
        Just uri -> return $ ValidatedRemoteSchemaDef name (EnvRecord (getTemplateFromUrl url) uri) hdrs fwdHdrs timeout customization
    -- case 2: URL is supplied as an environment variable
    (Nothing, Just urlEnv) -> do
      urlEnv' <- getUrlFromEnv env urlEnv
      return $ ValidatedRemoteSchemaDef name urlEnv' hdrs fwdHdrs timeout customization
    -- case 3: No url is supplied, throws an error 400
    (Nothing, Nothing) ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be empty"
    -- case 4: Both template and environment variables are supplied, throws an error 400
    (Just _, Just _) ->
      throw400 InvalidParams "both `url` and `url_from_env` can't be present"
  where
    hdrs = fromMaybe [] hdrC
    timeout = fromMaybe 60 mTimeout
    getTemplateFromUrl url = printTemplate $ unInputWebhook url

-- | See `resolveRemoteVariable` function. This data type is used
--   for validation of the session variable value
data SessionArgumentPresetInfo
  = SessionArgumentPresetScalar
  | SessionArgumentPresetEnum (Set.HashSet G.EnumValue)
  deriving (Show, Eq, Generic, Ord)

instance Hashable SessionArgumentPresetInfo

-- | Details required to resolve a "session variable preset" variable.
--
-- See Notes [Remote Schema Argument Presets] and [Remote Schema Permissions
-- Architecture] for additional information.
data RemoteSchemaVariable
  = SessionPresetVariable SessionVariable G.Name SessionArgumentPresetInfo
  | QueryVariable Variable
  | RemoteJSONValue G.GType J.Value
  deriving (Show, Eq, Generic, Ord)

instance Hashable RemoteSchemaVariable

-- | Extends 'G.InputValueDefinition' with an optional preset argument.
--
-- See Note [Remote Schema Argument Presets] for additional information.
data RemoteSchemaInputValueDefinition = RemoteSchemaInputValueDefinition
  { _rsitdDefinition :: G.InputValueDefinition,
    _rsitdPresetArgument :: Maybe (G.Value RemoteSchemaVariable)
  }
  deriving (Show, Eq, Generic, Ord)

instance Hashable RemoteSchemaInputValueDefinition

newtype RemoteSchemaIntrospection
  = RemoteSchemaIntrospection (HashMap G.Name (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition))
  deriving (Show, Eq, Generic, Hashable, Ord)

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
lookupType (RemoteSchemaIntrospection types) name = HashMap.lookup name types

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

-- remote relationships

-- A textual identifier for an entity on which remote relationships can be
-- defined. This is used in error messages and type name generation for
-- arguments in remote relationship fields to remote schemas (See
-- RemoteRelationship.Validate)
newtype LHSIdentifier = LHSIdentifier {getLHSIdentifier :: Text}
  deriving (Show, Eq, Generic)

remoteSchemaToLHSIdentifier :: RemoteSchemaName -> LHSIdentifier
remoteSchemaToLHSIdentifier = LHSIdentifier . toTxt

-- | Generates a valid graphql name from an arbitrary LHS identifier.
-- This is done by replacing all unrecognized characters by '_'. This
-- function still returns a @Maybe@ value, in cases we can't adjust
-- the raw text (such as the case of empty identifiers).
lhsIdentifierToGraphQLName :: LHSIdentifier -> Maybe G.Name
lhsIdentifierToGraphQLName (LHSIdentifier rawText) = G.mkName $ T.map adjust rawText
  where
    adjust c =
      if C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c
        then c
        else '_'

-- | Schema cache information for a table field targeting a remote schema.
data RemoteSchemaFieldInfo = RemoteSchemaFieldInfo
  { -- | Field name to which we'll map the remote in hasura; this becomes part
    --   of the hasura schema.
    _rrfiName :: RelName,
    -- | Input arguments to the remote field info; The '_rfiParamMap' will only
    --   include the arguments to the remote field that is being joined. The
    --   names of the arguments here are modified, it will be in the format of
    --   <Original Field Name>_remote_rel_<hasura table schema>_<hasura table name><remote relationship name>
    _rrfiParamMap :: HashMap G.Name RemoteSchemaInputValueDefinition,
    _rrfiRemoteFields :: RemoteFields,
    _rrfiRemoteSchema :: RemoteSchemaInfo,
    -- | The new input value definitions created for this remote field
    _rrfiInputValueDefinitions :: [G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition],
    -- | Name of the remote schema, that's used for joining
    _rrfiRemoteSchemaName :: RemoteSchemaName,
    -- | TODO: this one should be gone when 'validateRemoteRelationship'
    -- function is cleaned up
    _rrfiLHSIdentifier :: LHSIdentifier
  }
  deriving (Generic, Eq, Show)

-- FIXME: deduplicate this
graphQLValueToJSON :: G.Value Void -> J.Value
graphQLValueToJSON = \case
  G.VNull -> J.Null
  G.VInt i -> J.toJSON i
  G.VFloat f -> J.toJSON f
  G.VString t -> J.toJSON t
  G.VBoolean b -> J.toJSON b
  G.VEnum (G.EnumValue n) -> J.toJSON n
  G.VList values -> J.toJSON $ graphQLValueToJSON <$> values
  G.VObject objects -> J.toJSON $ graphQLValueToJSON <$> objects

$(J.deriveJSON hasuraJSON ''ValidatedRemoteSchemaDef)
$(J.deriveJSON hasuraJSON ''RemoteSchemaCustomizer)
$(J.deriveJSON hasuraJSON ''RemoteSchemaInfo)

instance (J.ToJSON remoteFieldInfo) => J.ToJSON (RemoteSchemaCtxG remoteFieldInfo) where
  toJSON RemoteSchemaCtx {..} =
    J.object
      $ [ "name" J..= _rscName,
          "info" J..= J.toJSON _rscInfo
        ]

instance J.ToJSON RemoteSchemaFieldInfo where
  toJSON RemoteSchemaFieldInfo {..} =
    J.object
      [ "name" J..= _rrfiName,
        "param_map" J..= fmap toJsonInpValInfo _rrfiParamMap,
        "remote_fields" J..= _rrfiRemoteFields,
        "remote_schema" J..= _rrfiRemoteSchema
      ]
    where
      toJsonInpValInfo (RemoteSchemaInputValueDefinition (G.InputValueDefinition desc name type' defVal _directives) _preset) =
        J.object
          [ "desc" J..= desc,
            "name" J..= name,
            "def_val" J..= fmap graphQLValueToJSON defVal,
            "type" J..= type'
          ]

$(makeLenses ''RemoteSchemaCtxG)
