module Hasura.GraphQL.Schema.Remote where

import           Hasura.Prelude

import           Language.GraphQL.Draft.Syntax       as G
import qualified Data.List.NonEmpty                  as NE

import           Hasura.GraphQL.Parser               as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.GraphQL.Parser          (FieldParser, Kind (..), Parser)

remoteFieldFullSchema
  :: forall n m
   . (MonadSchema n m, MonadError Text m)
  => SchemaDocument
  -> G.Name
  -> m (Parser 'Output n (G.SelectionSet NoFragments Variable))
remoteFieldFullSchema sdoc name =
  P.memoizeOn 'remoteFieldFullSchema name do
  fieldObjectType <-
    case lookupType sdoc name of
      Just (G.TypeDefinitionObject o) -> pure o
      _ -> throwError $ "object with " <> G.unName name <> " not found"
  fieldParser <- remoteSchemaObject sdoc fieldObjectType
  pure $ P.unsafeRawParser (P.pType fieldParser)

remoteSchemaObject
  :: forall n m
  . (MonadSchema n m, MonadError Text m)
  => SchemaDocument
  -> G.ObjectTypeDefinition
  -> m (Parser 'Output n ())
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name _interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
  let
    convert :: G.FieldDefinition -> m (FieldParser n ())
    convert (G.FieldDefinition _ _ argsDefinition gType _) =
      let
        addNullableList :: FieldParser n () -> FieldParser n ()
        addNullableList (P.FieldParser (Definition name' desc un (FieldInfo args typ)) parser)
          = P.FieldParser (Definition name' desc un (FieldInfo args (Nullable (TList typ)))) parser

        addNonNullableList :: FieldParser n () -> FieldParser n ()
        addNonNullableList (P.FieldParser (Definition name' desc un (FieldInfo args typ)) parser)
          = P.FieldParser (Definition name' desc un (FieldInfo args (NonNullable (TList typ)))) parser

        -- TODO add directives, deprecation
        convertType :: G.GType -> m (FieldParser n ())
        convertType gType' = do
            case gType' of
              G.TypeNamed (Nullability True) fieldTypeName -> do
                remoteFld <- remoteFieldFromName schemaDoc name fieldTypeName argsDefinition
                pure . P.nullableField $ remoteFld
              G.TypeList (Nullability True) gType'' ->
                pure .addNullableList =<< convertType gType''
              G.TypeNamed (Nullability False) fieldTypeName -> do
                remoteFld <- remoteFieldFromName schemaDoc name fieldTypeName argsDefinition
                pure . P.nonNullableField $ remoteFld
              G.TypeList (Nullability False) gType'' ->
                pure . addNonNullableList =<< convertType gType''
      in convertType gType
  subFieldParsers <- traverse convert subFields
  pure $ () <$ (P.selectionSet name description subFieldParsers)

remoteSchemaInputObject
  :: forall n m
  .  (MonadSchema n m, MonadError Text m)
  => SchemaDocument
  -> G.InputObjectTypeDefinition
  -> m (Parser 'Input n ())
remoteSchemaInputObject schemaDoc defn@(G.InputObjectTypeDefinition desc name _ valueDefns) =
  P.memoizeOn 'remoteSchemaInputObject defn do
  argsParser <- argumentsParser valueDefns schemaDoc
  pure $ P.object name desc argsParser

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
  :: forall n m
   . (MonadSchema n m, MonadError Text m)
  => SchemaDocument
  -> G.Name
  -> G.Name
  -> G.ArgumentsDefinition
  -> m (FieldParser n ())
remoteFieldFromName sdoc fieldName fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing -> throwError $ "Could not find type with name " <> G.unName fieldName
    Just typeDef -> remoteField sdoc fieldName argsDefns typeDef

inputValueDefinitionParser
  :: forall n m
   . (MonadSchema n m, MonadError Text m)
  => G.SchemaDocument
  -> G.InputValueDefinition
  -> m (InputFieldsParser n ())
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal) =
  let fieldConstructor :: forall k . 'Input <: k => Parser k n () -> InputFieldsParser n ()
      fieldConstructor = case maybeDefaultVal of
        Nothing -> field name desc
        Just defaultVal -> fieldWithDefault name desc defaultVal
      buildField
        :: G.GType
        -> (forall k . 'Input <: k => Parser k n () -> InputFieldsParser n ())
        -> m (InputFieldsParser n ())
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed _ typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> throwError $ "Could not find type with name " <> G.unName typeName
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar _ -> pure $ fieldConstructor' $ remoteFieldScalarParser name
               G.TypeDefinitionEnum defn -> pure $ fieldConstructor' $ remoteFieldEnumParser name defn
               G.TypeDefinitionObject _ -> throwError $ "expected input type, but got output type" -- couldn't find the equivalent error in Validate/Types.hs, so using a new error message
               G.TypeDefinitionInputObject defn ->
                 pure . fieldConstructor' =<< remoteSchemaInputObject schemaDoc defn
               G.TypeDefinitionUnion _ -> throwError $ "expected input type, but got output type"
               G.TypeDefinitionInterface _ -> throwError $ "expected input type, but got output type"
       G.TypeList _ subType -> buildField subType (fieldConstructor' . void . P.list)
  in buildField fieldType fieldConstructor

argumentsParser
  :: forall n m
  .  (MonadSchema n m, MonadError Text m)
  => G.ArgumentsDefinition
  -> G.SchemaDocument
  -> m (InputFieldsParser n ())
argumentsParser args schemaDoc = do
  pure () <$ mapM (inputValueDefinitionParser schemaDoc) args

remoteField
  :: forall n m
   . (MonadSchema n m, MonadError Text m)
  => SchemaDocument
  -> G.Name
  -> G.ArgumentsDefinition
  -> G.TypeDefinition
  -> m (FieldParser n ()) -- TODO return something useful, maybe?
remoteField sdoc fieldName argsDefn typeDefn = do
  -- TODO add directives
  argsParser <- argumentsParser argsDefn sdoc
  case typeDefn of
    G.TypeDefinitionObject objTypeDefn -> do
      remoteSchemaObj <- remoteSchemaObject sdoc objTypeDefn
      pure $ () <$ P.subselection fieldName (G._otdDescription objTypeDefn) argsParser remoteSchemaObj
    G.TypeDefinitionScalar (G.ScalarTypeDefinition desc _ _) ->
      pure $ P.selection fieldName desc argsParser $ remoteFieldScalarParser fieldName
    G.TypeDefinitionEnum enumTypeDefn@(G.EnumTypeDefinition desc _ _ _) ->
      pure $ P.selection fieldName desc argsParser $ remoteFieldEnumParser fieldName enumTypeDefn
    _ -> throwError $ "expected input type, but got output type"

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
                            ((mkDefinition (G.unEnumValue enumName) enumDesc P.EnumValueInfo),()))
                         $ valueDefns
  in P.enum name desc $ NE.fromList enumValDefns
