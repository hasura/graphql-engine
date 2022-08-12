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
    TypeRelationshipDefinition (..),
    RelationshipName (..),
    trdName,
    trdType,
    trdSource,
    trdRemoteTable,
    trdFieldMapping,

    -- * Schema cache
    AnnotatedCustomTypes (..),
    AnnotatedInputType (..),
    AnnotatedOutputType (..),
    AnnotatedObjectType (..),
    AnnotatedObjectFieldType (..),
    AnnotatedTypeRelationship (..),
    AnnotatedScalarType (..),
    ScalarWrapper (..),
  )
where

import Control.Lens.TH (makeLenses)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend
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
  | s == G.unName GName._Int = True
  | s == G.unName GName._Float = True
  | s == G.unName GName._String = True
  | s == G.unName GName._Boolean = True
  | s == G.unName GName._ID = True
  | otherwise = False

-- | A set of custom GraphQL types, sorted by "kind".
data CustomTypes = CustomTypes
  { _ctInputObjects :: [InputObjectTypeDefinition],
    _ctObjects :: [ObjectTypeDefinition],
    _ctScalars :: [ScalarTypeDefinition],
    _ctEnums :: [EnumTypeDefinition]
  }
  deriving (Show, Eq, Generic)

instance NFData CustomTypes

instance Cacheable CustomTypes

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

data ObjectTypeDefinition = ObjectTypeDefinition
  { _otdName :: ObjectTypeName,
    _otdDescription :: Maybe G.Description,
    _otdFields :: NonEmpty (ObjectFieldDefinition GraphQLType),
    _otdRelationships :: [TypeRelationshipDefinition]
  }
  deriving (Show, Eq, Generic)

instance NFData ObjectTypeDefinition

instance Cacheable ObjectTypeDefinition

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

defaultGraphQLScalars :: HashMap G.Name ScalarTypeDefinition
defaultGraphQLScalars = Map.fromList . map (\name -> (name, ScalarTypeDefinition name Nothing)) $ Set.toList GName.builtInScalars

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

data TypeRelationshipDefinition = TypeRelationshipDefinition
  { _trdName :: RelationshipName,
    _trdType :: RelType,
    -- TODO: replace this with RemoteRelationshipDefinition?
    -- As of now, we can't yet generalize this, due to the way async action
    -- queries' joins are implemented in GraphQL.Execute.Action (with an ad-hoc
    -- temporary table on postgres). If we are willing to change the way joins
    -- are performed, then we can replace this PG-specific code with the new and
    -- fancy generalized remote relationship code.
    _trdSource :: SourceName,
    _trdRemoteTable :: PG.QualifiedTable,
    _trdFieldMapping :: HashMap ObjectFieldName PG.PGCol
  }
  deriving (Show, Eq, Generic)

instance NFData TypeRelationshipDefinition

instance Cacheable TypeRelationshipDefinition

instance J.FromJSON TypeRelationshipDefinition where
  parseJSON = J.withObject "TypeRelationshipDefinition" $ \o ->
    TypeRelationshipDefinition
      <$> o .: "name"
      <*> o .: "type"
      <*> o .:? "source" .!= defaultSource
      <*> o .: "remote_table"
      <*> o .: "field_mapping"

-- | TODO: deduplicate this in favour of RelName
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
  | ASTReusedScalar G.Name (AnyBackend ScalarWrapper)
  deriving (Eq, Generic)

newtype ScalarWrapper b = ScalarWrapper {unwrapScalar :: (ScalarType b)}

deriving instance (Backend b) => Eq (ScalarWrapper b)

data AnnotatedOutputType
  = AOTObject AnnotatedObjectType
  | AOTScalar AnnotatedScalarType
  deriving (Generic)

data AnnotatedObjectType = AnnotatedObjectType
  { _aotName :: ObjectTypeName,
    _aotDescription :: Maybe G.Description,
    _aotFields :: NonEmpty (ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType)),
    _aotRelationships :: [AnnotatedTypeRelationship]
  }
  deriving (Generic)

data AnnotatedObjectFieldType
  = AOFTScalar AnnotatedScalarType
  | AOFTEnum EnumTypeDefinition
  | AOFTObject G.Name
  deriving (Generic)

-- TODO: deduplicate this with 'RemoteSourceFieldInfo'
data AnnotatedTypeRelationship = AnnotatedTypeRelationship
  { _atrName :: RelationshipName,
    _atrType :: RelType,
    _atrSource :: SourceName,
    _atrSourceConfig :: SourceConfig ('Postgres 'Vanilla),
    _atrSourceCustomization :: SourceTypeCustomization,
    -- TODO: see comment in 'TypeRelationship'
    _atrTableInfo :: TableInfo ('Postgres 'Vanilla),
    _atrFieldMapping :: HashMap ObjectFieldName (ColumnInfo ('Postgres 'Vanilla))
  }
  deriving (Generic)

-------------------------------------------------------------------------------
-- Template haskell derivation
-- ...and other instances that need to live here in a particular order, due to
-- GHC 9.0 TH changes...

$(J.deriveJSON hasuraJSON ''InputObjectFieldDefinition)
$(J.deriveJSON hasuraJSON ''InputObjectTypeDefinition)
$(J.deriveJSON hasuraJSON ''ObjectFieldDefinition)
$(J.deriveJSON hasuraJSON ''ScalarTypeDefinition)

$(J.deriveJSON hasuraJSON ''EnumValueDefinition)

$(J.deriveToJSON hasuraJSON ''TypeRelationshipDefinition)

instance J.ToJSON AnnotatedScalarType where
  toJSON = \case
    ASTCustom std ->
      J.object ["tag" .= J.String "ASTCustom", "contents" .= J.toJSON std]
    -- warning: can't be parsed back, as it does not include the
    -- backend-specific scalar information.
    ASTReusedScalar name _scalar ->
      J.object ["tag" .= J.String "ASTReusedScalar", "contents" .= J.toJSON name]

$(makeLenses ''TypeRelationshipDefinition)

$(J.deriveJSON hasuraJSON ''EnumTypeDefinition)

instance J.FromJSON CustomTypes where
  parseJSON = J.withObject "CustomTypes" \o ->
    CustomTypes
      <$> (o .:? "input_objects" .!= [])
      <*> (o .:? "objects" .!= [])
      <*> (o .:? "scalars" .!= [])
      <*> (o .:? "enums" .!= [])

instance J.FromJSON ObjectTypeDefinition where
  parseJSON = J.withObject "ObjectTypeDefinition" \o ->
    ObjectTypeDefinition
      <$> (o .: "name")
      <*> (o .:? "description")
      <*> (o .: "fields")
      <*> (o .:? "relationships" .!= [])

$(J.deriveToJSON hasuraJSON ''ObjectTypeDefinition)
$(J.deriveToJSON hasuraJSON ''CustomTypes)
$(J.deriveToJSON hasuraJSON ''AnnotatedInputType)
$(J.deriveToJSON hasuraJSON ''AnnotatedObjectFieldType)
$(J.deriveToJSON hasuraJSON ''AnnotatedTypeRelationship)
$(J.deriveToJSON hasuraJSON ''AnnotatedObjectType)
$(J.deriveToJSON hasuraJSON ''AnnotatedOutputType)
