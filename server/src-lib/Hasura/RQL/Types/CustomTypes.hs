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

import Autodocodec (HasCodec (codec), dimapCodec, optionalField', optionalFieldWith', optionalFieldWithDefault', optionalFieldWithOmittedDefault', requiredField', requiredFieldWith')
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLEnumValueCodec, graphQLFieldDescriptionCodec, graphQLFieldNameCodec, typeableName)
import Control.Lens.TH (makeLenses)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Text.Extended (ToTxt (..))
import Data.Typeable (Typeable)
import Hasura.Backends.Postgres.Instances.Types ()
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.SQL.AnyBackend
import Hasura.Table.Cache (GraphQLType (..), isListType, isNullableType)
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Metadata

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

instance HasCodec CustomTypes where
  codec =
    AC.object "CustomTypes"
      $ CustomTypes
      <$> optionalFieldWithOmittedDefault' "input_objects" []
      AC..= _ctInputObjects
        <*> optionalFieldWithOmittedDefault' "objects" []
      AC..= _ctObjects
        <*> optionalFieldWithOmittedDefault' "scalars" []
      AC..= _ctScalars
        <*> optionalFieldWithOmittedDefault' "enums" []
      AC..= _ctEnums

emptyCustomTypes :: CustomTypes
emptyCustomTypes = CustomTypes [] [] [] []

--------------------------------------------------------------------------------
-- Custom input objects

data InputObjectTypeDefinition = InputObjectTypeDefinition
  { _iotdName :: InputObjectTypeName,
    _iotdDescription :: Maybe G.Description,
    _iotdFields :: NonEmpty InputObjectFieldDefinition
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData InputObjectTypeDefinition

instance HasCodec InputObjectTypeDefinition where
  codec =
    AC.object "InputObjectTypeDefinition"
      $ InputObjectTypeDefinition
      <$> requiredField' "name"
      AC..= _iotdName
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _iotdDescription
        <*> requiredField' "fields"
      AC..= _iotdFields

newtype InputObjectTypeName = InputObjectTypeName {unInputObjectTypeName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData)

instance HasCodec InputObjectTypeName where
  codec = dimapCodec InputObjectTypeName unInputObjectTypeName codec

data InputObjectFieldDefinition = InputObjectFieldDefinition
  { _iofdName :: InputObjectFieldName,
    _iofdDescription :: Maybe G.Description,
    _iofdType :: GraphQLType
    -- TODO: support default values
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData InputObjectFieldDefinition

instance HasCodec InputObjectFieldDefinition where
  codec =
    AC.object "InputObjectFieldDefinition"
      $ InputObjectFieldDefinition
      <$> requiredField' "name"
      AC..= _iofdName
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _iofdDescription
        <*> requiredField' "type"
      AC..= _iofdType

newtype InputObjectFieldName = InputObjectFieldName {unInputObjectFieldName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData)

instance HasCodec InputObjectFieldName where
  codec = dimapCodec InputObjectFieldName unInputObjectFieldName codec

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

instance HasCodec ObjectTypeDefinition where
  codec =
    AC.object "ObjectTypeDefinition"
      $ ObjectTypeDefinition
      <$> requiredField' "name"
      AC..= _otdName
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _otdDescription
        <*> requiredField' "fields"
      AC..= _otdFields
        <*> optionalFieldWithOmittedDefault' "relationships" []
      AC..= _otdRelationships

newtype ObjectTypeName = ObjectTypeName {unObjectTypeName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData)

instance HasCodec ObjectTypeName where
  codec = dimapCodec ObjectTypeName unObjectTypeName codec

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

instance (HasCodec field, Typeable field) => HasCodec (ObjectFieldDefinition field) where
  codec =
    AC.object ("ObjectFieldDefinition_" <> typeableName @field)
      $ ObjectFieldDefinition
      <$> requiredField' "name"
      AC..= _ofdName
        <*> optionalField' "arguments"
      AC..= _ofdArguments
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _ofdDescription
        <*> requiredField' "type"
      AC..= _ofdType

newtype ObjectFieldName = ObjectFieldName {unObjectFieldName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, J.FromJSONKey, J.ToJSONKey, ToTxt, Generic, NFData)

instance HasCodec ObjectFieldName where
  codec = dimapCodec ObjectFieldName unObjectFieldName graphQLFieldNameCodec

--------------------------------------------------------------------------------
-- Custom scalars

data ScalarTypeDefinition = ScalarTypeDefinition
  { _stdName :: G.Name,
    _stdDescription :: Maybe G.Description
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData ScalarTypeDefinition

instance HasCodec ScalarTypeDefinition where
  codec =
    AC.object "ScalarTypeDefinition"
      $ ScalarTypeDefinition
      <$> requiredField' "name"
      AC..= _stdName
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _stdDescription

defaultGraphQLScalars :: HashMap G.Name ScalarTypeDefinition
defaultGraphQLScalars = HashMap.fromList . map (\name -> (name, ScalarTypeDefinition name Nothing)) $ Set.toList GName.builtInScalars

--------------------------------------------------------------------------------
-- Custom enums

data EnumTypeDefinition = EnumTypeDefinition
  { _etdName :: EnumTypeName,
    _etdDescription :: Maybe G.Description,
    _etdValues :: NonEmpty EnumValueDefinition
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData EnumTypeDefinition

instance HasCodec EnumTypeDefinition where
  codec =
    AC.object "EnumTypeDefinition"
      $ EnumTypeDefinition
      <$> requiredField' "name"
      AC..= _etdName
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _etdDescription
        <*> requiredField' "values"
      AC..= _etdValues

newtype EnumTypeName = EnumTypeName {unEnumTypeName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData)

instance HasCodec EnumTypeName where
  codec = dimapCodec EnumTypeName unEnumTypeName graphQLFieldNameCodec

data EnumValueDefinition = EnumValueDefinition
  { _evdValue :: G.EnumValue,
    _evdDescription :: Maybe G.Description,
    _evdIsDeprecated :: Maybe Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData EnumValueDefinition

instance HasCodec EnumValueDefinition where
  codec =
    AC.object "EnumValueDefinition"
      $ EnumValueDefinition
      <$> requiredFieldWith' "value" graphQLEnumValueCodec
      AC..= _evdValue
        <*> optionalFieldWith' "description" graphQLFieldDescriptionCodec
      AC..= _evdDescription
        <*> optionalField' "is_deprecated"
      AC..= _evdIsDeprecated

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
    _trdRemoteTable :: Postgres.QualifiedTable,
    _trdFieldMapping :: HashMap ObjectFieldName Postgres.PGCol
  }
  deriving (Show, Eq, Generic)

instance NFData TypeRelationshipDefinition

instance HasCodec TypeRelationshipDefinition where
  codec =
    AC.object "TypeRelationshipDefinition"
      $ TypeRelationshipDefinition
      <$> requiredField' "name"
      AC..= _trdName
        <*> requiredField' "type"
      AC..= _trdType
        <*> optionalFieldWithDefault' "source" defaultSource
      AC..= _trdSource
        <*> requiredField' "remote_table"
      AC..= _trdRemoteTable
        <*> requiredField' "field_mapping"
      AC..= _trdFieldMapping

instance J.FromJSON TypeRelationshipDefinition where
  parseJSON = J.withObject "TypeRelationshipDefinition" $ \o ->
    TypeRelationshipDefinition
      <$> o
      .: "name"
      <*> o
      .: "type"
      <*> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "remote_table"
      <*> o
      .: "field_mapping"

-- | TODO: deduplicate this in favour of RelName
newtype RelationshipName = RelationshipName {unRelationshipName :: G.Name}
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, ToTxt, Generic, NFData)

instance HasCodec RelationshipName where
  codec = dimapCodec RelationshipName unRelationshipName codec

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
  deriving (Eq, Ord, Generic)

data AnnotatedScalarType
  = ASTCustom ScalarTypeDefinition
  | ASTReusedScalar G.Name (AnyBackend ScalarWrapper)
  deriving (Eq, Ord, Generic)

data ScalarWrapper b = ScalarWrapper {unwrapScalar :: ScalarType b, parsingContext :: ScalarTypeParsingContext b}

deriving instance (Backend b) => Eq (ScalarWrapper b)

deriving instance (Backend b) => Ord (ScalarWrapper b)

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
    -- TODO: see comment in 'TypeRelationship'
    _atrTableName :: TableName ('Postgres 'Vanilla),
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
