{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.CustomTypes
  ( -- * Metadata
    GraphQLType (..),
    isListType,
    isNullableType,
    isInBuiltScalar,
    CustomTypes (..),
    emptyCustomTypes,

    -- ** Type definitions
    InputObjectTypeDefinition (..),
    InputObjectTypeName (..),
    InputObjectFieldDefinition (..),
    InputObjectFieldName (..),
    ObjectType,
    ObjectTypeDefinition (..),
    ObjectTypeName (..),
    ObjectFieldDefinition (..),
    ObjectFieldName (..),
    ScalarTypeDefinition (..),
    defaultGraphQLScalars,
    EnumTypeDefinition (..),
    EnumTypeName (..),
    EnumValueDefinition (..),

    -- ** Relationships
    TypeRelationship (..),
    RelationshipName (..),
    trName,
    trType,
    trSource,
    trRemoteTable,
    trFieldMapping,

    -- * Schema cache
    AnnotatedCustomTypes (..),
    AnnotatedInputType (..),
    AnnotatedOutputType (..),
    AnnotatedObjectType (..),
    AnnotatedObjectFieldType (..),
    AnnotatedScalarType (..),
    fieldTypeToScalarType,
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.Text qualified as T
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Table
import Hasura.SQL.Backend
import Language.GraphQL.Draft.Parser qualified as GParse
import Language.GraphQL.Draft.Printer qualified as GPrint
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as T

--------------------------------------------------------------------------------
-- Metadata

-- | A wrapper around 'G.GType' which allows us to define custom JSON
-- instances.
--
-- TODO: this name is ambiguous, and conflicts with
-- Hasura.RQL.DDL.RemoteSchema.Permission.GraphQLType; it should perhaps be
-- renamed, made internal to this module, or removed altogether?
newtype GraphQLType = GraphQLType {unGraphQLType :: G.GType}
  deriving (Show, Eq, Generic, NFData, Cacheable)

instance J.ToJSON GraphQLType where
  toJSON = J.toJSON . T.run . GPrint.graphQLType . unGraphQLType

instance J.FromJSON GraphQLType where
  parseJSON =
    J.withText "GraphQLType" $ \t ->
      case GParse.parseGraphQLType t of
        Left _ -> fail $ "not a valid GraphQL type: " <> T.unpack t
        Right a -> return $ GraphQLType a

isListType :: GraphQLType -> Bool
isListType = coerce G.isListType

isNullableType :: GraphQLType -> Bool
isNullableType = coerce G.isNullable

isInBuiltScalar :: Text -> Bool
isInBuiltScalar s
  | s == G.unName intScalar = True
  | s == G.unName floatScalar = True
  | s == G.unName stringScalar = True
  | s == G.unName boolScalar = True
  | s == G.unName idScalar = True
  | otherwise = False

-- | A set of custom GraphQL types, sorted by "kind".
data CustomTypes = CustomTypes
  { _ctInputObjects :: [InputObjectTypeDefinition],
    _ctObjects :: [ObjectType],
    _ctScalars :: [ScalarTypeDefinition],
    _ctEnums :: [EnumTypeDefinition]
  }
  deriving (Show, Eq, Generic)

instance NFData CustomTypes

instance Cacheable CustomTypes

instance J.FromJSON CustomTypes where
  parseJSON = J.withObject "CustomTypes" \o ->
    CustomTypes
      <$> (o .:? "input_objects" .!= [])
      <*> (o .:? "objects" .!= [])
      <*> (o .:? "scalars" .!= [])
      <*> (o .:? "enums" .!= [])

emptyCustomTypes :: CustomTypes
emptyCustomTypes = CustomTypes [] [] [] []

--------------------------------------------------------------------------------
-- Custom input objects

data InputObjectTypeDefinition = InputObjectTypeDefinition
  { _iotdName :: InputObjectTypeName,
    _iotdDescription :: Maybe G.Description,
    _iotdFields :: NonEmpty InputObjectFieldDefinition
  }
  deriving (Show, Eq, Generic)

instance NFData InputObjectTypeDefinition

instance Cacheable InputObjectTypeDefinition

newtype InputObjectTypeName = InputObjectTypeName {unInputObjectTypeName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data InputObjectFieldDefinition = InputObjectFieldDefinition
  { _iofdName :: InputObjectFieldName,
    _iofdDescription :: Maybe G.Description,
    _iofdType :: GraphQLType
    -- TODO: support default values
  }
  deriving (Show, Eq, Generic)

instance NFData InputObjectFieldDefinition

instance Cacheable InputObjectFieldDefinition

newtype InputObjectFieldName = InputObjectFieldName {unInputObjectFieldName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

--------------------------------------------------------------------------------
-- Custom objects

-- | TODO: remove this, either by specializing 'ObjectTypeDefinition', or by
-- moving the ties to Postgres outside of this module.
type ObjectType =
  ObjectTypeDefinition GraphQLType QualifiedTable PGCol

data ObjectTypeDefinition field remoteTable remoteField = ObjectTypeDefinition
  { _otdName :: ObjectTypeName,
    _otdDescription :: Maybe G.Description,
    _otdFields :: NonEmpty (ObjectFieldDefinition field),
    _otdRelationships :: [TypeRelationship remoteTable remoteField]
  }
  deriving (Show, Eq, Generic)

instance (NFData field, NFData remoteTable, NFData remoteField) => NFData (ObjectTypeDefinition field remoteTable remoteField)

instance (Cacheable field, Cacheable remoteTable, Cacheable remoteField) => Cacheable (ObjectTypeDefinition field remoteTable remoteField)

instance (J.FromJSON a, J.FromJSON b, J.FromJSON c) => J.FromJSON (ObjectTypeDefinition a b c) where
  parseJSON = J.withObject "ObjectTypeDefinition" \o ->
    ObjectTypeDefinition
      <$> (o .: "name")
      <*> (o .:? "description")
      <*> (o .: "fields")
      <*> (o .:? "relationships" .!= [])

newtype ObjectTypeName = ObjectTypeName {unObjectTypeName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data ObjectFieldDefinition field = ObjectFieldDefinition
  { _ofdName :: ObjectFieldName,
    -- we don't care about field arguments/directives
    -- as objectDefinitions types are only used as the return
    -- type of a webhook response and as such the extra
    -- context will be hard to pass to the webhook
    _ofdArguments :: Maybe J.Value,
    _ofdDescription :: Maybe G.Description,
    _ofdType :: field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

instance (NFData field) => NFData (ObjectFieldDefinition field)

instance (Cacheable field) => Cacheable (ObjectFieldDefinition field)

newtype ObjectFieldName = ObjectFieldName {unObjectFieldName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey, ToTxt, Generic, NFData, Cacheable)

--------------------------------------------------------------------------------
-- Custom scalars

data ScalarTypeDefinition = ScalarTypeDefinition
  { _stdName :: G.Name,
    _stdDescription :: Maybe G.Description
  }
  deriving (Show, Eq, Generic)

instance NFData ScalarTypeDefinition

instance Cacheable ScalarTypeDefinition

defaultGraphQLScalars :: [ScalarTypeDefinition]
defaultGraphQLScalars = do
  name <- [intScalar, floatScalar, stringScalar, boolScalar, idScalar]
  pure $ ScalarTypeDefinition name Nothing

--------------------------------------------------------------------------------
-- Custom enums

data EnumTypeDefinition = EnumTypeDefinition
  { _etdName :: EnumTypeName,
    _etdDescription :: Maybe G.Description,
    _etdValues :: NonEmpty EnumValueDefinition
  }
  deriving (Show, Eq, Generic)

instance NFData EnumTypeDefinition

instance Cacheable EnumTypeDefinition

newtype EnumTypeName = EnumTypeName {unEnumTypeName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

data EnumValueDefinition = EnumValueDefinition
  { _evdValue :: G.EnumValue,
    _evdDescription :: Maybe G.Description,
    _evdIsDeprecated :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance NFData EnumValueDefinition

instance Cacheable EnumValueDefinition

--------------------------------------------------------------------------------
-- Relationships

data TypeRelationship table field = TypeRelationship
  { _trName :: RelationshipName,
    _trType :: RelType,
    -- TODO: replace this with RemoteRelationshipDefinition?
    _trSource :: SourceName,
    _trRemoteTable :: table,
    _trFieldMapping :: HashMap ObjectFieldName field
  }
  deriving (Show, Eq, Generic)

instance (NFData t, NFData f) => NFData (TypeRelationship t f)

instance (Cacheable t, Cacheable f) => Cacheable (TypeRelationship t f)

instance (J.FromJSON t, J.FromJSON f) => J.FromJSON (TypeRelationship t f) where
  parseJSON = J.withObject "Object" $ \o ->
    TypeRelationship <$> o .: "name"
      <*> o .: "type"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "remote_table"
      <*> o .: "field_mapping"

newtype RelationshipName = RelationshipName {unRelationshipName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData, Cacheable)

--------------------------------------------------------------------------------
-- Schema cache

-- | While we do not persist resolved types in the schema cache, they are used
-- when building the cache to validate and resolve actions and their
-- relationships.
data AnnotatedCustomTypes = AnnotatedCustomTypes
  { _actInputTypes :: HashMap G.Name AnnotatedInputType,
    _actObjectTypes :: HashMap G.Name AnnotatedObjectType
  }

instance Semigroup AnnotatedCustomTypes where
  AnnotatedCustomTypes no1 o1 <> AnnotatedCustomTypes no2 o2 =
    AnnotatedCustomTypes (no1 <> no2) (o1 <> o2)

instance Monoid AnnotatedCustomTypes where
  mempty = AnnotatedCustomTypes mempty mempty

data AnnotatedInputType
  = NOCTScalar AnnotatedScalarType
  | NOCTEnum EnumTypeDefinition
  | NOCTInputObject InputObjectTypeDefinition
  deriving (Eq, Generic)

data AnnotatedScalarType
  = ASTCustom ScalarTypeDefinition
  | ASTReusedScalar G.Name (ScalarType ('Postgres 'Vanilla))
  deriving (Eq, Generic)

data AnnotatedOutputType
  = AOTObject AnnotatedObjectType
  | AOTScalar AnnotatedScalarType
  deriving (Generic)

-- TODO: replace this with equivalent code from RemoteRelationship.
data AnnotatedObjectType = AnnotatedObjectType
  { _aotDefinition :: ObjectTypeDefinition (G.GType, AnnotatedObjectFieldType) (TableInfo ('Postgres 'Vanilla)) (ColumnInfo ('Postgres 'Vanilla)),
    _aotSource :: Maybe (SourceName, SourceConfig ('Postgres 'Vanilla))
  }
  deriving (Generic)

data AnnotatedObjectFieldType
  = AOFTScalar !AnnotatedScalarType
  | AOFTEnum !EnumTypeDefinition
  | AOFTObject !G.Name
  deriving (Generic)

-- FIXME: why is this required, and why is it here?
fieldTypeToScalarType :: AnnotatedObjectFieldType -> PGScalarType
fieldTypeToScalarType = \case
  AOFTEnum _ -> PGText
  AOFTScalar annotatedScalar -> annotatedScalarToPgScalar annotatedScalar
  AOFTObject _ -> PGJSON
  where
    annotatedScalarToPgScalar = \case
      ASTReusedScalar _ scalarType -> scalarType
      ASTCustom ScalarTypeDefinition {..} ->
        if
            | _stdName == idScalar -> PGText
            | _stdName == intScalar -> PGInteger
            | _stdName == floatScalar -> PGFloat
            | _stdName == stringScalar -> PGText
            | _stdName == boolScalar -> PGBoolean
            | otherwise -> PGJSON

-------------------------------------------------------------------------------
-- Template haskell derivation

$(J.deriveJSON hasuraJSON ''InputObjectFieldDefinition)
$(J.deriveJSON hasuraJSON ''InputObjectTypeDefinition)
$(J.deriveJSON hasuraJSON ''ObjectFieldDefinition)
$(J.deriveJSON hasuraJSON ''ScalarTypeDefinition)
$(J.deriveJSON hasuraJSON ''EnumTypeDefinition)
$(J.deriveJSON hasuraJSON ''EnumValueDefinition)

$(J.deriveToJSON hasuraJSON ''CustomTypes)
$(J.deriveToJSON hasuraJSON ''ObjectTypeDefinition)
$(J.deriveToJSON hasuraJSON ''TypeRelationship)
$(J.deriveToJSON hasuraJSON ''AnnotatedInputType)
$(J.deriveToJSON hasuraJSON ''AnnotatedScalarType)
$(J.deriveToJSON hasuraJSON ''AnnotatedOutputType)
$(J.deriveToJSON hasuraJSON ''AnnotatedObjectType)
$(J.deriveToJSON hasuraJSON ''AnnotatedObjectFieldType)

$(makeLenses ''TypeRelationship)
