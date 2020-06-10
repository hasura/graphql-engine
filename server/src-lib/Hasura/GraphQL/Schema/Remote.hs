module Hasura.GraphQL.Schema.Remote where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax       as G
import qualified Language.GraphQL.Draft.Printer      as GP
import qualified Language.GraphQL.Draft.Printer.Text as GPT
import qualified Data.Aeson                          as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd          as OMap
import qualified Data.Vector                         as V

import           Hasura.GraphQL.Parser               as P

import           Hasura.GraphQL.Parser          (FieldParser, Kind (..), Parser, Schema (..))
import           Hasura.GraphQL.Parser.Class

-- IntrospectionResult

remoteSchemaObject
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> ObjectTypeDefinition
  -> Parser 'Output n () -- (G.SelectionSet NoFragments Variable)
remoteSchemaObject sdoc objType =
  let
    name = G._otdName objType
    description = G._otdDescription objType
    subfields = G._otdFieldsDefinition objType -- [G.FieldDefinition], need [FieldParser n a]
    convert :: G.FieldDefinition -> FieldParser n ()
    convert fieldDef =
      -- TODO add arguments and directives
      case G._fldType fieldDef of
        G.TypeNamed (Nullability True) fieldTypeName ->
          remoteFieldFromName sdoc (G._fldName fieldDef) fieldTypeName
        _ -> _todo
  in
    -- TODO return the parsed selection set.
    () <$ (P.selectionSet name description $ map convert subfields)

lookupType :: SchemaDocument -> G.Name -> Maybe TypeDefinition
lookupType (SchemaDocument types) name = find (\tp -> getNamedTyp tp == name) types
  where
    getNamedTyp :: G.TypeDefinition -> G.Name
    getNamedTyp ty = case ty of
      G.TypeDefinitionScalar t      -> G._stdName t
      G.TypeDefinitionObject t      -> G._otdName t
      G.TypeDefinitionInterface t   -> G._itdName t
      G.TypeDefinitionUnion t       -> G._utdName t
      G.TypeDefinitionEnum t        -> G._etdName t
      G.TypeDefinitionInputObject t -> G._iotdName t

remoteFieldFromName
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> G.Name
  -> G.Name
  -> FieldParser n ()
remoteFieldFromName sdoc fieldName fieldTypeName =
  case lookupType sdoc fieldTypeName of
    Nothing -> _todo
    Just typeDef -> remoteField sdoc fieldName typeDef

remoteField
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> G.Name
  -> TypeDefinition
  -> FieldParser n () -- TODO return something useful, maybe?
remoteField sdoc fieldName (TypeDefinitionObject objType) =
  -- TODO add arguments and directivez
  P.subselection_ fieldName Nothing (remoteSchemaObject sdoc objType)

{-
data FieldDefinition = FieldDefinition
  { _fldDescription         :: Maybe Description
  , _fldName                :: Name
  , _fldArgumentsDefinition :: ArgumentsDefinition
  , _fldType                :: GType
  , _fldDirectives          :: [Directive Void]
  } deriving (Ord, Show, Eq, Lift, Generic)
-}

{-
query {
  requestWeather (city: "Berlin") {
    temperature
  }
  currentTime
}
-}
