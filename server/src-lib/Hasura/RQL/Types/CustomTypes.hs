module Hasura.RQL.Types.CustomTypes
  ( CustomTypes(..)
  , emptyCustomTypes
  , GraphQLType(..)
  , isListType
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
  , ObjectTypeName(..)
  , ObjectTypeDefinition(..)
  , ObjectType
  , NonObjectCustomType(..)
  , NonObjectTypeMap
  , AnnotatedObjectFieldType(..)
  , AnnotatedObjectType
  , AnnotatedObjects
  , AnnotatedCustomTypes(..)
  , emptyAnnotatedCustomTypes
  ) where

import           Control.Lens.TH                     (makeLenses)
import           Instances.TH.Lift                   ()
import           Language.Haskell.TH.Syntax          (Lift)

import qualified Data.Aeson                          as J
import qualified Data.Aeson.Casing                   as J
import qualified Data.Aeson.TH                       as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.List.NonEmpty                  as NEList
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Parser       as GParse
import qualified Language.GraphQL.Draft.Printer      as GPrint
import qualified Language.GraphQL.Draft.Printer.Text as GPrintText
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.Incremental                  (Cacheable)
import           Hasura.Prelude
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
isListType (GraphQLType ty) =
  case ty of
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
  = InputObjectTypeName { unInputObjectTypeName :: G.Name }
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
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (NFData a) => NFData (ObjectFieldDefinition a)
instance (Cacheable a) => Cacheable (ObjectFieldDefinition a)
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
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''TypeRelationship)

newtype ObjectTypeName
  = ObjectTypeName { unObjectTypeName :: G.Name }
  deriving ( Show, Eq, Ord, Hashable, J.FromJSON, J.FromJSONKey, DQuote
           , J.ToJSONKey, J.ToJSON, Lift, Generic, NFData, Cacheable)

data ObjectTypeDefinition a b c
  = ObjectTypeDefinition
  { _otdName          :: !ObjectTypeName
  , _otdDescription   :: !(Maybe G.Description)
  , _otdFields        :: !(NonEmpty (ObjectFieldDefinition a))
  , _otdRelationships :: !(Maybe (NonEmpty (TypeRelationship b c)))
  } deriving (Show, Eq, Lift, Generic)
instance (NFData a, NFData b, NFData c) => NFData (ObjectTypeDefinition a b c)
instance (Cacheable a, Cacheable b, Cacheable c) => Cacheable (ObjectTypeDefinition a b c)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ObjectTypeDefinition)

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdName        :: !G.Name
  , _stdDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Lift, Generic)
instance NFData ScalarTypeDefinition
instance Cacheable ScalarTypeDefinition
instance Hashable ScalarTypeDefinition
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ScalarTypeDefinition)

newtype EnumTypeName
  = EnumTypeName { unEnumTypeName :: G.Name }
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

type ObjectType =
  ObjectTypeDefinition GraphQLType QualifiedTable PGCol

data CustomTypes
  = CustomTypes
  { _ctInputObjects :: !(Maybe [InputObjectTypeDefinition])
  , _ctObjects      :: !(Maybe [ObjectType])
  , _ctScalars      :: !(Maybe [ScalarTypeDefinition])
  , _ctEnums        :: !(Maybe [EnumTypeDefinition])
  } deriving (Show, Eq, Lift, Generic)
instance NFData CustomTypes
instance Cacheable CustomTypes
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CustomTypes)

emptyCustomTypes :: CustomTypes
emptyCustomTypes = CustomTypes Nothing Nothing Nothing Nothing

data NonObjectCustomType
  = NOCTScalar !ScalarTypeDefinition
  | NOCTEnum !EnumTypeDefinition
  | NOCTInputObject !InputObjectTypeDefinition
  deriving (Show, Eq, Lift)
$(J.deriveJSON J.defaultOptions ''NonObjectCustomType)

type NonObjectTypeMap = Map.HashMap G.Name NonObjectCustomType

data AnnotatedObjectFieldType
  = AOFTScalar !ScalarTypeDefinition
  | AOFTEnum !EnumTypeDefinition
  deriving (Show, Eq)
$(J.deriveToJSON J.defaultOptions ''AnnotatedObjectFieldType)

type AnnotatedObjectType =
  ObjectTypeDefinition (G.GType, AnnotatedObjectFieldType) TableInfo PGColumnInfo

type AnnotatedObjects = Map.HashMap G.Name AnnotatedObjectType

data AnnotatedCustomTypes
  = AnnotatedCustomTypes
    { _actNonObjects :: !NonObjectTypeMap
    , _actObjects    :: !AnnotatedObjects
    } deriving (Show, Eq)

emptyAnnotatedCustomTypes :: AnnotatedCustomTypes
emptyAnnotatedCustomTypes =
  AnnotatedCustomTypes mempty mempty
