module Hasura.RQL.Types.CustomTypes
  ( CustomTypes(..)
  , emptyCustomTypes
  , GraphQLType(..)
  , isListType
  , isListType'
  , EnumTypeName(..)
  , EnumValueDefinition(..)
  , EnumTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , InputObjectFieldName(..)
  , InputObjectFieldDefinition(..)
  , InputObjectTypeName(..)
  , InputObjectTypeDefinition(..)
  , ObjectFieldName(..)
  , ObjectFieldDefinition(..)
  , RelationshipName(..)
  , TypeRelationship(..)
  , trName, trType, trRemoteTable, trFieldMapping
  , TypeRelationshipDefinition
  , ObjectTypeName(..)
  , ObjectTypeDefinition(..)
  , CustomTypeName
  , CustomTypeDefinition(..)
  , CustomTypeDefinitionMap
  , OutputFieldTypeInfo(..)
  , AnnotatedObjectType(..)
  , AnnotatedObjects
  , AnnotatedRelationship
  , NonObjectTypeMap(..)
  ) where

import           Control.Lens.TH                     (makeLenses)
import           Language.Haskell.TH.Syntax          (Lift)

import qualified Data.Aeson                          as J
import qualified Data.Aeson.Casing                   as J
import qualified Data.Aeson.TH                       as J
import qualified Data.Text                           as T

import qualified Data.HashMap.Strict                 as Map
import qualified Data.List.NonEmpty                  as NEList
import           Instances.TH.Lift                   ()
import qualified Language.GraphQL.Draft.Parser       as GParse
import qualified Language.GraphQL.Draft.Printer      as GPrint
import qualified Language.GraphQL.Draft.Printer.Text as GPrintText
import qualified Language.GraphQL.Draft.Syntax       as G

import qualified Hasura.GraphQL.Validate.Types       as VT

import           Hasura.Incremental                  (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Instances                ()
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common             (RelType)
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Types

newtype GraphQLType
  = GraphQLType { unGraphQLType :: G.GType }
  deriving (Show, Eq, Lift, Generic, NFData, Cacheable)

instance J.ToJSON GraphQLType where
  toJSON = J.toJSON . GPrintText.render GPrint.graphQLType . unGraphQLType

instance J.FromJSON GraphQLType where
  parseJSON =
    J.withText "GraphQLType" $ \t ->
    case GParse.parseGraphQLType t of
      Left _  -> fail $ "not a valid GraphQL type: " <> T.unpack t
      Right a -> return $ GraphQLType a

isListType :: GraphQLType -> Bool
isListType (GraphQLType ty) = isListType' ty

isListType' :: G.GType -> Bool
isListType' = \case
  G.TypeList _ _  -> True
  G.TypeNamed _ _ -> False

newtype InputObjectFieldName
  = InputObjectFieldName { unInputObjectFieldName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, DQuote, Lift, Generic, NFData, Cacheable)

data InputObjectFieldDefinition
  = InputObjectFieldDefinition
  { _iofdName        :: !InputObjectFieldName
  , _iofdDescription :: !(Maybe G.Description)
  , _iofdType        :: !GraphQLType
  -- TODO: default
  } deriving (Show, Eq, Lift, Generic)
instance NFData InputObjectFieldDefinition
instance Cacheable InputObjectFieldDefinition
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''InputObjectFieldDefinition)

newtype InputObjectTypeName
  = InputObjectTypeName { unInputObjectTypeName :: G.NamedType }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, DQuote, Lift, Generic, NFData, Cacheable)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdName        :: !InputObjectTypeName
  , _iotdDescription :: !(Maybe G.Description)
  , _iotdFields      :: !(NEList.NonEmpty InputObjectFieldDefinition)
  } deriving (Show, Eq, Lift, Generic)
instance NFData InputObjectTypeDefinition
instance Cacheable InputObjectTypeDefinition
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''InputObjectTypeDefinition)

newtype ObjectFieldName
  = ObjectFieldName { unObjectFieldName :: G.Name }
  deriving ( Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, DQuote
           , J.FromJSONKey, J.ToJSONKey, Lift, Generic, NFData, Cacheable)

data ObjectFieldDefinition
  = ObjectFieldDefinition
  { _ofdName        :: !ObjectFieldName
  -- we don't care about field arguments/directives
  -- as objectDefinitions types are only used as the return
  -- type of a webhook response and as such the extra
  -- context will be hard to pass to the webhook
  , _ofdArguments   :: !(Maybe J.Value)
  , _ofdDescription :: !(Maybe G.Description)
  , _ofdType        :: !GraphQLType
  } deriving (Show, Eq, Lift, Generic)
instance NFData ObjectFieldDefinition
instance Cacheable ObjectFieldDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ObjectFieldDefinition)

newtype RelationshipName
  = RelationshipName { unRelationshipName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, DQuote, Lift, Generic, NFData, Cacheable)

data TypeRelationship t f
  = TypeRelationship
  { _trName         :: !RelationshipName
  , _trType         :: !RelType
  , _trRemoteTable  :: !t
  , _trFieldMapping :: !(Map.HashMap ObjectFieldName f)
  } deriving (Show, Eq, Lift, Generic)
instance (NFData t, NFData f) => NFData (TypeRelationship t f)
instance (Cacheable t, Cacheable f) => Cacheable (TypeRelationship t f)
$(makeLenses ''TypeRelationship)

type TypeRelationshipDefinition =
  TypeRelationship QualifiedTable PGCol

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''TypeRelationship)

newtype ObjectTypeName
  = ObjectTypeName { unObjectTypeName :: G.NamedType }
  deriving ( Show, Eq, Ord, Hashable, J.FromJSON, J.FromJSONKey, DQuote
           , J.ToJSONKey, J.ToJSON, Lift, Generic, NFData, Cacheable)

data ObjectTypeDefinition
  = ObjectTypeDefinition
  { _otdName          :: !ObjectTypeName
  , _otdDescription   :: !(Maybe G.Description)
  , _otdFields        :: !(NEList.NonEmpty ObjectFieldDefinition)
  , _otdRelationships :: !(Maybe [TypeRelationshipDefinition])
  } deriving (Show, Eq, Lift, Generic)
instance NFData  ObjectTypeDefinition
instance Cacheable   ObjectTypeDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ObjectTypeDefinition)

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdName        :: !G.NamedType
  , _stdDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Lift, Generic)
instance NFData ScalarTypeDefinition
instance Cacheable ScalarTypeDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ScalarTypeDefinition)

newtype EnumTypeName
  = EnumTypeName { unEnumTypeName :: G.NamedType }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, DQuote, Lift, Generic, NFData, Cacheable)

data EnumValueDefinition
  = EnumValueDefinition
  { _evdValue        :: !G.EnumValue
  , _evdDescription  :: !(Maybe G.Description)
  , _evdIsDeprecated :: !(Maybe Bool)
  } deriving (Show, Eq, Lift, Generic)
instance NFData EnumValueDefinition
instance Cacheable EnumValueDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EnumValueDefinition)

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdName        :: !EnumTypeName
  , _etdDescription :: !(Maybe G.Description)
  , _etdValues      :: !(NEList.NonEmpty EnumValueDefinition)
  } deriving (Show, Eq, Lift, Generic)
instance NFData EnumTypeDefinition
instance Cacheable EnumTypeDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EnumTypeDefinition)

data CustomTypeDefinition
  = CustomTypeScalar !ScalarTypeDefinition
  | CustomTypeEnum !EnumTypeDefinition
  | CustomTypeInputObject !InputObjectTypeDefinition
  | CustomTypeObject !ObjectTypeDefinition
  deriving (Show, Eq, Lift)
$(J.deriveJSON J.defaultOptions ''CustomTypeDefinition)

type CustomTypeDefinitionMap = Map.HashMap G.NamedType CustomTypeDefinition
newtype CustomTypeName
  = CustomTypeName { unCustomTypeName :: G.NamedType }
  deriving (Show, Eq, Hashable, J.ToJSONKey, J.FromJSONKey)

data CustomTypes
  = CustomTypes
  { _ctInputObjects :: !(Maybe [InputObjectTypeDefinition])
  , _ctObjects      :: !(Maybe [ObjectTypeDefinition])
  , _ctScalars      :: !(Maybe [ScalarTypeDefinition])
  , _ctEnums        :: !(Maybe [EnumTypeDefinition])
  } deriving (Show, Eq, Lift, Generic)
instance NFData CustomTypes
instance Cacheable CustomTypes
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CustomTypes)

emptyCustomTypes :: CustomTypes
emptyCustomTypes = CustomTypes Nothing Nothing Nothing Nothing

type AnnotatedRelationship =
  TypeRelationship TableInfo PGColumnInfo

data OutputFieldTypeInfo
  = OutputFieldScalar !VT.ScalarTyInfo
  | OutputFieldEnum !VT.EnumTyInfo
  deriving (Show, Eq)

data AnnotatedObjectType
  = AnnotatedObjectType
  { _aotDefinition      :: !ObjectTypeDefinition
  , _aotAnnotatedFields :: !(Map.HashMap ObjectFieldName (G.GType, OutputFieldTypeInfo))
  , _aotRelationships   :: !(Map.HashMap RelationshipName AnnotatedRelationship)
  } deriving (Show, Eq)

instance J.ToJSON AnnotatedObjectType where
  toJSON = J.toJSON . show

type AnnotatedObjects = Map.HashMap ObjectTypeName AnnotatedObjectType

newtype NonObjectTypeMap
  = NonObjectTypeMap { unNonObjectTypeMap :: VT.TypeMap }
  deriving (Show, Eq, Semigroup, Monoid)

instance J.ToJSON NonObjectTypeMap where
  toJSON = J.toJSON . show
