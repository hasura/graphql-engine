module Hasura.GraphQL.Schema.Remote where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax       as G
import qualified Data.List.NonEmpty                  as NE

import           Hasura.GraphQL.Parser               as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.GraphQL.Parser          (FieldParser, Kind (..), Parser)

remoteFieldFullSchema
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> G.Name
  -> Parser 'Output n (G.SelectionSet NoFragments Variable)
remoteFieldFullSchema sdoc name =
  let fieldObjectType = case lookupType sdoc name of
                          Just (G.TypeDefinitionObject o) -> o
                          _ -> _errorNoSuchObjectInSchema
      fieldParser = remoteSchemaObject sdoc fieldObjectType
  in unsafeRawParser (pType fieldParser)

{-
query {
  currentWeather (city:"berlin") {
    temperature
  }
}
-}

remoteSchemaObject
  :: forall n
   . MonadParse n
  => SchemaDocument
  -> G.ObjectTypeDefinition
  -> Parser 'Output n ()
remoteSchemaObject schemaDoc (G.ObjectTypeDefinition description name _interfaces _directives subFields) =
  let
    convert :: G.FieldDefinition -> FieldParser n ()
    convert (G.FieldDefinition _ _ argsDefinition gType _) =
      let
        addNullableList :: FieldParser n () -> FieldParser n ()
        addNullableList (P.FieldParser (Definition name desc un (FieldInfo args typ)) parser)
          = P.FieldParser (Definition name desc un (FieldInfo args (Nullable (TList typ)))) parser
        addNonNullableList :: FieldParser n () -> FieldParser n ()
        addNonNullableList (P.FieldParser (Definition name desc un (FieldInfo args typ)) parser)
          = P.FieldParser (Definition name desc un (FieldInfo args (NonNullable (TList typ)))) parser
        -- TODO add directives, deprecation
        convertType :: G.GType -> FieldParser n ()
        convertType gType' =
          case gType' of
            G.TypeNamed (Nullability True) fieldTypeName ->
              P.nullableField $ remoteFieldFromName schemaDoc name fieldTypeName argsDefinition
            G.TypeList (Nullability True) gType'' ->
              addNullableList $ convertType gType''
            G.TypeNamed (Nullability False) fieldTypeName ->
              P.nonNullableField $ remoteFieldFromName schemaDoc name fieldTypeName argsDefinition
            G.TypeList (Nullability False) gType'' ->
              addNonNullableList $ convertType gType''
      in convertType gType
  in
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
  :: forall n
   . MonadParse n
  => G.SchemaDocument
  -> G.InputValueDefinition
  -> InputFieldsParser n ()
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal) =
  let fieldConstructor :: forall k . 'Input <: k => Parser k n () -> InputFieldsParser n ()
      fieldConstructor = case maybeDefaultVal of
        Nothing -> field name desc
        Just defaultVal -> fieldWithDefault name desc defaultVal
      buildField
        :: G.GType
        -> (forall k . 'Input <: k => Parser k n () -> InputFieldsParser n ())
        -> InputFieldsParser n ()
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed _ typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> _throwNotFoundNameInSchemaDoc
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar _ -> fieldConstructor' $ remoteFieldScalarParser name
               G.TypeDefinitionEnum defn -> fieldConstructor' $ remoteFieldEnumParser name defn
               G.TypeDefinitionObject _ -> _throwInvalidKindError
               G.TypeDefinitionInputObject defn -> fieldConstructor' $ remoteSchemaInputObject schemaDoc defn
               G.TypeDefinitionUnion _ -> _throwInvalidKindError
               G.TypeDefinitionInterface _ -> _throwInvalidKindError
       G.TypeList _ subType -> buildField subType (fieldConstructor' . void . P.list)
  in buildField fieldType fieldConstructor

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
  -> FieldParser n () -- TODO return something useful, maybe?
remoteField sdoc fieldName argsDefn typeDefn =
  -- TODO add directives
  let argsParser = argumentsParser argsDefn sdoc
  in
    case typeDefn of
    G.TypeDefinitionObject objTypeDefn -> do
      P.subselection fieldName (G._otdDescription objTypeDefn) argsParser $ remoteSchemaObject sdoc objTypeDefn
      pure ()
    G.TypeDefinitionScalar scalarTypeDefn@(G.ScalarTypeDefinition desc _ _) ->
      P.selection fieldName desc argsParser $ remoteFieldScalarParser fieldName
    G.TypeDefinitionEnum enumTypeDefn@(G.EnumTypeDefinition desc _ _ valueDefns) ->
      P.selection fieldName desc argsParser $ remoteFieldEnumParser fieldName enumTypeDefn
    _ -> _errorInputTypeInOutputPosition

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
