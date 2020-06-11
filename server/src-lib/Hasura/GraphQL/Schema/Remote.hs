module Hasura.GraphQL.Schema.Remote where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax       as G
import qualified Data.List.NonEmpty                  as NE

import           Hasura.GraphQL.Parser               as P

import           Hasura.GraphQL.Parser          (FieldParser, Kind (..), Parser)

remoteSchemaObject
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> G.ObjectTypeDefinition
  -> Parser 'Output n () -- (G.SelectionSet NoFragments Variable)
remoteSchemaObject schemaDoc (G.ObjectTypeDefinition description name _ _ subFields) =
  let
    convert :: G.FieldDefinition -> FieldParser n ()
    convert (G.FieldDefinition _ _ argsDefinition gType _) =
      -- TODO add directives
      case gType of
        G.TypeNamed (Nullability True) fieldTypeName ->
          remoteFieldFromName schemaDoc name fieldTypeName argsDefinition
  in
    -- TODO return the parsed selection set.
    () <$ (P.selectionSet name description $ map convert subFields)

remoteSchemaInputObject
  :: forall n
  .  MonadParse n
  => SchemaDocument
  -> G.InputObjectTypeDefinition
  -> Parser 'Input n ()
remoteSchemaInputObject schemaDoc (G.InputObjectTypeDefinition desc name _ valueDefns) =
  P.object name desc $ argumentsParser valueDefns schemaDoc

lookupType :: SchemaDocument -> G.Name -> Maybe G.TypeDefinition
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
  -> G.ArgumentsDefinition
  -> FieldParser n ()
remoteFieldFromName sdoc fieldName fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing -> _throwNameDoesNotExistError
    Just typeDef -> remoteField sdoc fieldName argsDefns typeDef

inputValueDefinitionParser
  :: (MonadParse n)
  => G.SchemaDocument
  -> G.InputValueDefinition
  -> InputFieldsParser n ()
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal)  =
  case maybeDefaultVal of
    Nothing ->
      case fieldType of
        G.TypeNamed _ typeName ->
          case lookupType schemaDoc typeName of
            Nothing -> _throwNotFoundNameInSchemaDoc
            Just typeDef ->
              case typeDef of
                G.TypeDefinitionScalar _ -> field name desc $ remoteFieldScalarParser name
                G.TypeDefinitionEnum defn -> field name desc $ remoteFieldEnumParser name defn
                G.TypeDefinitionObject _ -> _throwInvalidKindError
                G.TypeDefinitionInputObject defn -> field name desc $ remoteSchemaInputObject schemaDoc defn
                G.TypeDefinitionUnion _ -> _throwInvalidKindError
                G.TypeDefinitionInterface _ -> _throwInvalidKindError
        G.TypeList _ _ -> _todo
    Just defaultVal ->
      case fieldType of
        G.TypeNamed _ typeName ->
          case lookupType schemaDoc typeName of
            Nothing -> _throwNotFoundNameInSchemaDoc
            Just typeDef ->
              case typeDef of
                G.TypeDefinitionScalar _ -> fieldWithDefault name desc defaultVal $ remoteFieldScalarParser name
                G.TypeDefinitionEnum defn -> fieldWithDefault name desc defaultVal $ remoteFieldEnumParser name defn
                G.TypeDefinitionObject _ -> _throwInvalidKindError
                G.TypeDefinitionInputObject defn -> fieldWithDefault name desc defaultVal $ remoteSchemaInputObject schemaDoc defn
                G.TypeDefinitionUnion _ -> _throwInvalidKindError
                G.TypeDefinitionInterface _ -> _throwInvalidKindError
        G.TypeList _ _ -> _todo

argumentsParser
  :: (MonadParse n)
  => G.ArgumentsDefinition
  -> G.SchemaDocument
  -> InputFieldsParser n ()
argumentsParser args schemaDoc = do
  () <$ traverse (inputValueDefinitionParser schemaDoc) args

remoteField
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> G.Name
  -> G.ArgumentsDefinition
  -> G.TypeDefinition
  -> FieldParser n ()-- TODO return something useful, maybe?
remoteField sdoc fieldName argsDefn typeDefn =
  -- TODO add arguments and directives
  let argsParser = argumentsParser argsDefn sdoc
  in
    case typeDefn of
    G.TypeDefinitionObject objTypeDefn -> do
      P.subselection fieldName (G._otdDescription objTypeDefn) argsParser $ remoteSchemaObject sdoc objTypeDefn
      pure ()
    G.TypeDefinitionScalar (G.ScalarTypeDefinition desc _ _) ->
      P.selection fieldName desc argsParser $
        case G.unName fieldName of
          "Boolean" -> P.boolean $> ()
          "Int" -> P.int $> ()
          "Float" -> P.float $> ()
          "String" -> P.string $> ()
            --        _ -> P.selection_ fieldName desc $                          TODO: Unknown Scalar

    G.TypeDefinitionEnum (G.EnumTypeDefinition desc _ _ valueDefns) ->
      let enumValDefns = map (\(G.EnumValueDefinition enumDesc enumName _) ->
                                ((mkDefinition (G.unEnumValue enumName) enumDesc EnumValueInfo),()))
                         $ valueDefns
      in
        P.selection fieldName desc argsParser $ P.enum fieldName desc $ NE.fromList enumValDefns

    G.TypeDefinitionInputObject inpObjTypDefn@(G.InputObjectTypeDefinition desc name directives valueDefns) ->
      -- not needed,as it is an input type, but handle it properly
      _todo

remoteFieldScalarParser
  :: MonadParse n
  => G.Name
  -> Parser 'Both n ()
remoteFieldScalarParser name =
  case G.unName name of
    "Boolean" -> P.boolean $> ()
    "Int" -> P.int $> ()
    "Float" -> P.float $> ()
    "String" -> P.string $> ()
    --        _ -> TODO: Unknown Scalar

remoteFieldEnumParser
  :: MonadParse n
  => G.Name
  -> G.EnumTypeDefinition
  -> Parser 'Both n ()
remoteFieldEnumParser name (G.EnumTypeDefinition desc _ _ valueDefns) =
  let enumValDefns = map (\(G.EnumValueDefinition enumDesc enumName _) ->
                            ((mkDefinition (G.unEnumValue enumName) enumDesc EnumValueInfo),()))
                         $ valueDefns
  in P.enum name desc $ NE.fromList enumValDefns
