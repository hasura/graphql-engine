module Hasura.RQL.Types.CustomTypes
  ( CustomTypes(..)
  , GraphQLType(..)
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
  , ObjectTypeName(..)
  , ObjectTypeDefinition(..)
  ) where

import           Language.Haskell.TH.Syntax          (Lift)

import qualified Data.Aeson                          as J
import qualified Data.Aeson.Casing                   as J
import qualified Data.Aeson.TH                       as J
import qualified Data.Text                           as T

import qualified Data.List.NonEmpty                  as NEList
import qualified Language.GraphQL.Draft.Parser       as GParse
import qualified Language.GraphQL.Draft.Printer      as GPrint
import qualified Language.GraphQL.Draft.Printer.Text as GPrintText
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.Prelude

newtype GraphQLType
  = GraphQLType { unGraphQLType :: G.GType }
  deriving (Show, Eq, Lift)

instance J.ToJSON GraphQLType where
  toJSON = J.toJSON . GPrintText.render GPrint.graphQLType . unGraphQLType

instance J.FromJSON GraphQLType where
  parseJSON =
    J.withText "GraphQLType" $ \t ->
    case GParse.parseGraphQLType t of
      Left _  -> fail $ "not a valid GraphQL type: " <> T.unpack t
      Right a -> return $ GraphQLType a

newtype InputObjectFieldName
  = InputObjectFieldName { unInputObjectFieldName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, Lift)

data InputObjectFieldDefinition
  = InputObjectFieldDefinition
  { _iofdName        :: !InputObjectFieldName
  , _iofdDescription :: !(Maybe G.Description)
  , _iofdType        :: !GraphQLType
  -- TODO: default
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''InputObjectFieldDefinition)

newtype InputObjectTypeName
  = InputObjectTypeName { unInputObjectTypeName :: G.NamedType }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, Lift)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdName        :: !InputObjectTypeName
  , _iotdDescription :: !(Maybe G.Description)
  , _iotdFields      :: !(NEList.NonEmpty InputObjectFieldDefinition)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 5 J.snakeCase) ''InputObjectTypeDefinition)

newtype ObjectFieldName
  = ObjectFieldName { unObjectFieldName :: G.Name }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, Lift)

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
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ObjectFieldDefinition)

newtype ObjectTypeName
  = ObjectTypeName { unObjectTypeName :: G.NamedType }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, Lift)

data ObjectTypeDefinition
  = ObjectTypeDefinition
  { _otdName        :: !ObjectTypeName
  , _otdDescription :: !(Maybe G.Description)
  , _otdFields      :: !(NEList.NonEmpty ObjectFieldDefinition)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ObjectTypeDefinition)

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdName        :: !G.NamedType
  , _stdDescription :: !(Maybe G.Description)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''ScalarTypeDefinition)

newtype EnumTypeName
  = EnumTypeName { unEnumTypeName :: G.NamedType }
  deriving (Show, Eq, Ord, Hashable, J.FromJSON, J.ToJSON, Lift)

data EnumValueDefinition
  = EnumValueDefinition
  { _evdValue        :: !G.EnumValue
  , _evdDescription  :: !(Maybe G.Description)
  , _evdIsDeprecated :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EnumValueDefinition)

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdName        :: !EnumTypeName
  , _etdDescription :: !(Maybe G.Description)
  , _etdValues      :: !(NEList.NonEmpty EnumValueDefinition)
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EnumTypeDefinition)

data CustomTypes
  = CustomTypes
  { _ctInputObjects :: !(Maybe [InputObjectTypeDefinition])
  , _ctObjects      :: !(Maybe [ObjectTypeDefinition])
  , _ctScalars      :: !(Maybe [ScalarTypeDefinition])
  , _ctEnums        :: !(Maybe [EnumTypeDefinition])
  } deriving (Show, Eq, Lift)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''CustomTypes)
