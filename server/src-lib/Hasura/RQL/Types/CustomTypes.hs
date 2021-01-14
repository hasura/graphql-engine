module Hasura.RQL.Types.CustomTypes
  ( CustomTypes(..)
  , emptyCustomTypes
  , GraphQLType(..)
  , isListType
  , EnumTypeName(..)
  , EnumValueDefinition(..)
  , EnumTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , intScalar, floatScalar, stringScalar, boolScalar, idScalar
  , defaultScalars
  , InputObjectFieldName(..)
  , InputObjectFieldDefinition(..)
  , InputObjectTypeName(..)
  , InputObjectTypeDefinition(..)
  , ObjectFieldName(..)
  , ObjectFieldDefinition(..)
  , RelationshipName(..)
  , TypeRelationship(..)
  , trName, trType, trRemoteTable, trFieldMapping
  , ObjectTypeName(..)
  , ObjectTypeDefinition(..)
  , ObjectType
  , AnnotatedScalarType(..)
  , NonObjectCustomType(..)
  , NonObjectTypeMap
  , AnnotatedObjectFieldType(..)
  , fieldTypeToScalarType
  , AnnotatedObjectType(..)
  , AnnotatedObjects
  , AnnotatedCustomTypes(..)
  , emptyAnnotatedCustomTypes
  ) where

import           Control.Lens.TH                    (makeLenses)
import           Data.Text.Extended

import qualified Data.Aeson                         as J
import qualified Data.Aeson.Casing                  as J
import qualified Data.Aeson.TH                      as J
import qualified Data.HashMap.Strict                as Map
import qualified Data.List.NonEmpty                 as NEList
import qualified Data.Text                          as T
import qualified Language.GraphQL.Draft.Parser      as GParse
import qualified Language.GraphQL.Draft.Printer     as GPrint
import qualified Language.GraphQL.Draft.Syntax      as G
import qualified Text.Builder                       as T

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                 (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common            (RelType, ScalarType, SourceConfig, SourceName,
                                                     defaultSource)
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend

newtype GraphQLType
  = GraphQLType { unGraphQLType :: G.GType }
  deriving (Show, Eq, Generic, NFData, Cacheable)

instance J.ToJSON GraphQLType where
  toJSON = J.toJSON . T.run . GPrint.graphQLType . unGraphQLType

instance J.FromJSON GraphQLType where
  parseJSON =
    J.withText "GraphQLType" $ \t ->
    case GParse.parseGraphQLType t of
      Left _  -> fail $ "not a valid GraphQL type: " <> T.unpack t
      Right a -> return $ GraphQLType a

isListType :: GraphQLType -> Bool
isListType (GraphQLType ty) = G.isListType ty

newtype InputObjectFieldName
  = InputObjectFieldName { unInputObjectFieldName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data InputObjectFieldDefinition
  = InputObjectFieldDefinition
  { _iofdName        :: !InputObjectFieldName
  , _iofdDescription :: !(Maybe G.Description)
  , _iofdType        :: !GraphQLType
  -- TODO (from master): default
  } deriving (Show, Eq, Generic)
instance NFData InputObjectFieldDefinition
instance Cacheable InputObjectFieldDefinition
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''InputObjectFieldDefinition)

newtype InputObjectTypeName
  = InputObjectTypeName { unInputObjectTypeName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdName        :: !InputObjectTypeName
  , _iotdDescription :: !(Maybe G.Description)
  , _iotdFields      :: !(NEList.NonEmpty InputObjectFieldDefinition)
  } deriving (Show, Eq, Generic)
instance NFData InputObjectTypeDefinition
instance Cacheable InputObjectTypeDefinition
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''InputObjectTypeDefinition)

newtype ObjectFieldName
  = ObjectFieldName { unObjectFieldName :: G.Name }
  deriving ( Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt
           , J.FromJSONKey, J.ToJSONKey, Generic, NFData, Cacheable)

data ObjectFieldDefinition a
  = ObjectFieldDefinition
  { _ofdName        :: !ObjectFieldName
  -- we don't care about field arguments/directives
  -- as objectDefinitions types are only used as the return
  -- type of a webhook response and as such the extra
  -- context will be hard to pass to the webhook
  , _ofdArguments   :: !(Maybe J.Value)
  , _ofdDescription :: !(Maybe G.Description)
  , _ofdType        :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (NFData a) => NFData (ObjectFieldDefinition a)
instance (Cacheable a) => Cacheable (ObjectFieldDefinition a)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ObjectFieldDefinition)

newtype RelationshipName
  = RelationshipName { unRelationshipName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data TypeRelationship t f
  = TypeRelationship
  { _trName         :: !RelationshipName
  , _trType         :: !RelType
  , _trSource       :: !SourceName
  , _trRemoteTable  :: !t
  , _trFieldMapping :: !(Map.HashMap ObjectFieldName f)
  } deriving (Show, Eq, Generic)
instance (NFData t, NFData f) => NFData (TypeRelationship t f)
instance (Cacheable t, Cacheable f) => Cacheable (TypeRelationship t f)
$(makeLenses ''TypeRelationship)
$(J.deriveToJSON (J.aesonDrop 3 J.snakeCase) ''TypeRelationship)

instance (J.FromJSON t, J.FromJSON f) => J.FromJSON (TypeRelationship t f) where
  parseJSON = J.withObject "Object" $ \o ->
    TypeRelationship <$> o J..: "name"
                     <*> o J..: "type"
                     <*> o J..:? "source" J..!= defaultSource
                     <*> o J..: "remote_table"
                     <*> o J..: "field_mapping"

newtype ObjectTypeName
  = ObjectTypeName { unObjectTypeName :: G.Name }
  deriving ( Show, Eq, Ord, Hashable, J.FromJSON, J.FromJSONKey, ToTxt
           , J.ToJSONKey, J.ToJSON, Generic, NFData, Cacheable)

data ObjectTypeDefinition a b c
  = ObjectTypeDefinition
  { _otdName          :: !ObjectTypeName
  , _otdDescription   :: !(Maybe G.Description)
  , _otdFields        :: !(NonEmpty (ObjectFieldDefinition a))
  , _otdRelationships :: !(Maybe (NonEmpty (TypeRelationship b c)))
  } deriving (Show, Eq, Generic)
instance (NFData a, NFData b, NFData c) => NFData (ObjectTypeDefinition a b c)
instance (Cacheable a, Cacheable b, Cacheable c) => Cacheable (ObjectTypeDefinition a b c)
$(J.deriveToJSON (J.aesonDrop 4 J.snakeCase) ''ObjectTypeDefinition)

instance (J.FromJSON a, J.FromJSON b, J.FromJSON c) => J.FromJSON (ObjectTypeDefinition a b c) where
  parseJSON = J.withObject "ObjectTypeDefinition" \obj -> do
    name <- obj J..: "name"
    desc <- obj J..:? "description"
    fields <- obj J..: "fields"
    relationships <- obj J..:? "relationships"
    -- We need to do the below because pre-PDV, '[]' was a legal value
    -- for relationships because the type was `(Maybe [TypeRelationshipDefinition])`,
    -- In PDV, the type was changed to `(Maybe (NonEmpty (TypeRelationship b c)))`
    -- which breaks on `[]` for the `relationships` field, to be backwards compatible
    -- this `FromJSON` instance is written by hand and `[]` sets `_otdRelationships`
    -- to `Nothing`
    return $ ObjectTypeDefinition name desc fields (nonEmpty =<< relationships)

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdName        :: !G.Name
  , _stdDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Generic)
instance NFData ScalarTypeDefinition
instance Cacheable ScalarTypeDefinition
instance Hashable ScalarTypeDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ScalarTypeDefinition)

-- default scalar names
intScalar, floatScalar, stringScalar, boolScalar, idScalar :: G.Name
intScalar    = $$(G.litName "Int")
floatScalar  = $$(G.litName "Float")
stringScalar = $$(G.litName "String")
boolScalar   = $$(G.litName "Boolean")
idScalar     = $$(G.litName "ID")

defaultScalars :: [ScalarTypeDefinition]
defaultScalars =
  map (`ScalarTypeDefinition` Nothing)
  [intScalar, floatScalar, stringScalar, boolScalar, idScalar]

newtype EnumTypeName
  = EnumTypeName { unEnumTypeName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data EnumValueDefinition
  = EnumValueDefinition
  { _evdValue        :: !G.EnumValue
  , _evdDescription  :: !(Maybe G.Description)
  , _evdIsDeprecated :: !(Maybe Bool)
  } deriving (Show, Eq, Generic)
instance NFData EnumValueDefinition
instance Cacheable EnumValueDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EnumValueDefinition)

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdName        :: !EnumTypeName
  , _etdDescription :: !(Maybe G.Description)
  , _etdValues      :: !(NEList.NonEmpty EnumValueDefinition)
  } deriving (Show, Eq, Generic)
instance NFData EnumTypeDefinition
instance Cacheable EnumTypeDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EnumTypeDefinition)

type ObjectType =
  ObjectTypeDefinition GraphQLType QualifiedTable PGCol

data CustomTypes
  = CustomTypes
  { _ctInputObjects :: !(Maybe [InputObjectTypeDefinition])
  , _ctObjects      :: !(Maybe [ObjectType])
  , _ctScalars      :: !(Maybe [ScalarTypeDefinition])
  , _ctEnums        :: !(Maybe [EnumTypeDefinition])
  } deriving (Show, Eq, Generic)
instance NFData CustomTypes
instance Cacheable CustomTypes
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CustomTypes)

emptyCustomTypes :: CustomTypes
emptyCustomTypes = CustomTypes Nothing Nothing Nothing Nothing

data AnnotatedScalarType
  = ASTCustom !ScalarTypeDefinition
  | forall b . (b ~ 'Postgres) => ASTReusedScalar !G.Name !(ScalarType b)
  -- TODO: at a later stage, we shouldn't hardcode reused scalar types to be
  -- Postgres, but allow scalar types to come from any kind of backend.  In
  -- order words, the restriction (b ~ 'Postgres) should be relaxed.
deriving instance Eq AnnotatedScalarType
instance J.ToJSON AnnotatedScalarType where
  toJSON (ASTCustom std)           = J.toJSON std
  toJSON (ASTReusedScalar name st) = J.object ["name" J..= name, "type" J..= st]

data NonObjectCustomType
  = NOCTScalar !AnnotatedScalarType
  | NOCTEnum !EnumTypeDefinition
  | NOCTInputObject !InputObjectTypeDefinition
  deriving (Generic)
deriving instance Eq NonObjectCustomType
instance J.ToJSON NonObjectCustomType where
  toJSON = J.genericToJSON $ J.defaultOptions

type NonObjectTypeMap = Map.HashMap G.Name NonObjectCustomType

data AnnotatedObjectFieldType
  = AOFTScalar !AnnotatedScalarType
  | AOFTEnum !EnumTypeDefinition
  deriving (Generic)
instance J.ToJSON AnnotatedObjectFieldType where
  toJSON = J.genericToJSON $ J.defaultOptions

fieldTypeToScalarType :: AnnotatedObjectFieldType -> PGScalarType
fieldTypeToScalarType = \case
  AOFTEnum _                 -> PGText
  AOFTScalar annotatedScalar -> annotatedScalarToPgScalar annotatedScalar
  where
    annotatedScalarToPgScalar = \case
      ASTReusedScalar _ scalarType     -> scalarType
      ASTCustom ScalarTypeDefinition{..} ->
        if | _stdName == idScalar     -> PGText
           | _stdName == intScalar    -> PGInteger
           | _stdName == floatScalar  -> PGFloat
           | _stdName == stringScalar -> PGText
           | _stdName == boolScalar   -> PGBoolean
           | otherwise                -> PGJSON

data AnnotatedObjectType b
  = AnnotatedObjectType
  { _aotDefinition :: !(ObjectTypeDefinition (G.GType, AnnotatedObjectFieldType) (TableInfo b) (ColumnInfo b))
  , _aotSource     :: !(Maybe (SourceConfig b))
  } deriving (Generic)
instance J.ToJSON (AnnotatedObjectType 'Postgres) where
  toJSON = J.toJSON . _aotDefinition

type AnnotatedObjects b = Map.HashMap G.Name (AnnotatedObjectType b)

data AnnotatedCustomTypes (b :: BackendType)
  = AnnotatedCustomTypes
    { _actNonObjects :: !NonObjectTypeMap
    , _actObjects    :: !(AnnotatedObjects b)
    }

emptyAnnotatedCustomTypes :: AnnotatedCustomTypes backend
emptyAnnotatedCustomTypes =
  AnnotatedCustomTypes mempty mempty
