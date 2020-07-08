module Hasura.GraphQL.Schema.Remote
  ( buildRemoteParser
  , remoteFieldFullSchema
  , inputValueDefinitionParser
  , lookupObject
  , lookupType
  , lookupScalar
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types

import           Language.GraphQL.Draft.Syntax       as G
import qualified Data.List.NonEmpty                  as NE
import           Data.Type.Equality

import           Hasura.GraphQL.Parser               as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P

buildRemoteParser
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaCtx
  -> m ( [P.FieldParser n (RemoteSchemaInfo, Field NoFragments Variable)]
       , Maybe [P.FieldParser n (RemoteSchemaInfo, Field NoFragments Variable)]
       , Maybe [P.FieldParser n (RemoteSchemaInfo, Field NoFragments Variable)])
buildRemoteParser (RemoteSchemaCtx _name (sdoc, query_root, mutation_root, subscription_root) info) = do
  queryT <- makeParsers query_root
  mutationT <- traverse makeParsers mutation_root
  subscriptionT <- traverse makeParsers subscription_root
  return (queryT, mutationT, subscriptionT)
  where
    makeFieldParser :: G.FieldDefinition -> m (P.FieldParser n (RemoteSchemaInfo, Field NoFragments Variable))
    makeFieldParser fieldDef = do
      fp <- remoteField' sdoc fieldDef
      return $ do
        raw <- P.unsafeRawField (P.fDefinition fp)
        return (info, raw)
    makeParsers :: G.Name -> m [P.FieldParser n (RemoteSchemaInfo, Field NoFragments Variable)]
    makeParsers rootName =
      case lookupType sdoc rootName of
        Just (G.TypeDefinitionObject o) ->
          traverse makeFieldParser $ _otdFieldsDefinition o
        _ -> throw500 "Root type of unexpected type" -- TODO show rootName

-- | 'remoteFieldFullSchema' takes the 'SchemaDocument' and a 'G.Name' and will
--   return a 'SelectionSet' parser if the 'G.Name' is found and is a 'TypeDefinitionObject',
--   otherwise, an error will be thrown.
remoteFieldFullSchema
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaDocument
  -> G.Name
  -> m (Parser 'Output n (G.SelectionSet NoFragments Variable))
remoteFieldFullSchema sdoc name =
  P.memoizeOn 'remoteFieldFullSchema name do
  fieldObjectType <-
    case lookupType sdoc name of
      Just (G.TypeDefinitionObject o) -> pure o
      _ -> throw500 $ "object with " <> G.unName name <> " not found"
  fieldParser <- remoteSchemaObject sdoc fieldObjectType
  pure $ P.unsafeRawParser (P.pType fieldParser)

remoteField'
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaDocument
  -> G.FieldDefinition
  -> m (FieldParser n ())
remoteField' schemaDoc (G.FieldDefinition _ name argsDefinition gType _) =
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
            pure . addNullableList =<< convertType gType''
          G.TypeNamed (Nullability False) fieldTypeName -> do
            remoteFld <- remoteFieldFromName schemaDoc name fieldTypeName argsDefinition
            pure . P.nonNullableField $ remoteFld
          G.TypeList (Nullability False) gType'' ->
            pure . addNonNullableList =<< convertType gType''
  in convertType gType

-- | 'remoteSchemaObject' returns a output parser for a given 'ObjectTypeDefinition'.
remoteSchemaObject
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaDocument
  -> G.ObjectTypeDefinition
  -> m (Parser 'Output n ())
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) subFields
  implements <- traverse (remoteSchemaInterface schemaDoc) $ catMaybes (map (lookupInterface schemaDoc) interfaces)
  pure $ () <$ P.selectionSetObject name description subFieldParsers implements

objectsImplementingInterface
  :: SchemaDocument
  -> G.Name
  -> [G.ObjectTypeDefinition]
objectsImplementingInterface (SchemaDocument tps) interfaceName = catMaybes $ fmap go tps
  where
    go :: TypeDefinition -> Maybe G.ObjectTypeDefinition
    go (TypeDefinitionObject obj@(G.ObjectTypeDefinition _desc _name interfaces _directives _fields))
      | interfaceName `elem` interfaces = Just obj
    go _ = Nothing

interfaceImplementingInterface
  :: SchemaDocument
  -> G.Name
  -> [G.InterfaceTypeDefinition]
interfaceImplementingInterface (SchemaDocument tps) interfaceName = catMaybes $ fmap go tps
  where
    go :: TypeDefinition -> Maybe G.InterfaceTypeDefinition
    go (TypeDefinitionInterface iface@(G.InterfaceTypeDefinition _desc _name _directives _fields))
      | interfaceName `elem` [] = Just iface -- TODO where to get the interfaces from? this seems to be an oversight in graphql-parser-hs
    go _ = Nothing

-- | 'remoteSchemaInterface' returns a output parser for a given 'InterfaceTypeDefinition'.
remoteSchemaInterface
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaDocument
  -> G.InterfaceTypeDefinition
  -> m (Parser 'Output n ())
remoteSchemaInterface schemaDoc defn@(G.InterfaceTypeDefinition description name _directives fields) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) fields
  objs :: [Parser 'Output n ()] <- traverse (remoteSchemaObject schemaDoc) $
    objectsImplementingInterface schemaDoc name
  ifaces :: [Parser 'Output n ()] <- traverse (remoteSchemaInterface schemaDoc) $
    interfaceImplementingInterface schemaDoc name
  pure $ () <$ (P.selectionSetInterface name description subFieldParsers objs ifaces)

-- | 'remoteSchemaUnion' returns a output parser for a given 'UnionTypeDefinition'.
remoteSchemaUnion
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaDocument
  -> G.UnionTypeDefinition
  -> m (Parser 'Output n ())
remoteSchemaUnion schemaDoc defn@(G.UnionTypeDefinition description name _directives objectNames) =
  P.memoizeOn 'remoteSchemaObject defn do
  objs :: [Parser 'Output n ()] <- traverse (remoteSchemaObject schemaDoc) $
    catMaybes $ map (lookupObject schemaDoc) objectNames
  pure $ () <$ (P.selectionSetUnion name description objs)

-- | remoteSchemaInputObject returns an input parser for a given 'G.InputObjectTypeDefinition'
remoteSchemaInputObject
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
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

lookupObject :: SchemaDocument -> G.Name -> Maybe G.ObjectTypeDefinition
lookupObject (SchemaDocument types) name = go types
  where
    go :: [TypeDefinition] -> Maybe G.ObjectTypeDefinition
    go ((G.TypeDefinitionObject t):tps)
      | G._otdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupInterface :: SchemaDocument -> G.Name -> Maybe G.InterfaceTypeDefinition
lookupInterface (SchemaDocument types) name = go types
  where
    go :: [TypeDefinition] -> Maybe G.InterfaceTypeDefinition
    go ((G.TypeDefinitionInterface t):tps)
      | G._itdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupScalar :: SchemaDocument -> G.Name -> Maybe G.ScalarTypeDefinition
lookupScalar (SchemaDocument types) name = go types
  where
    go :: [TypeDefinition] -> Maybe G.ScalarTypeDefinition
    go ((G.TypeDefinitionScalar t):tps)
      | G._stdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

-- | 'remoteFieldFromName' accepts a GraphQL name and searches for its definition
--   in the 'SchemaDocument'.
remoteFieldFromName
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaDocument
  -> G.Name
  -> G.Name
  -> G.ArgumentsDefinition
  -> m (FieldParser n ())
remoteFieldFromName sdoc fieldName fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing -> throw500 $ "Could not find type with name " <> G.unName fieldName
    Just typeDef -> remoteField sdoc fieldName argsDefns typeDef

-- | 'inputValuefinitionParser' accepts a 'G.InputValueDefinition' and will return an
--   'InputFieldsParser' for it. If a non 'Input' GraphQL type is found in the 'type' of
--    the 'InputValueDefinition' then an error will be thrown.
inputValueDefinitionParser
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => G.SchemaDocument
  -> G.InputValueDefinition
  -> m (InputFieldsParser n (G.Value Variable))
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal) =
  let fieldConstructor :: forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Value Variable)
      fieldConstructor parser =
        let wrappedParser :: Parser k n (Value Variable)
            wrappedParser =
              P.Parser
                { P.pType   = P.pType parser
                , P.pParser = \value -> P.pParser parser value $> castWith (P.inputParserInput @k) value
                }
        in case maybeDefaultVal of
          Nothing -> field name desc wrappedParser
          Just defaultVal -> fieldWithDefault name desc defaultVal wrappedParser
      buildField
        :: G.GType
        -> (forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (G.Value Variable))
        -> m (InputFieldsParser n (G.Value Variable))
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed _ typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> throw500 $ "Could not find type with name " <> G.unName typeName -- should it be 400 instead?
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar (G.ScalarTypeDefinition _ name' _) ->
                 fieldConstructor' <$> remoteFieldScalarParser name'
               G.TypeDefinitionEnum defn -> pure $ fieldConstructor' $ remoteFieldEnumParser defn
               G.TypeDefinitionObject _ -> throw500 $ "expected input type, but got output type" -- couldn't find the equivalent error in Validate/Types.hs, so using a new error message
               G.TypeDefinitionInputObject defn ->
                 pure . fieldConstructor' =<< remoteSchemaInputObject schemaDoc defn
               G.TypeDefinitionUnion _ -> throw500 $ "expected input type, but got output type"
               G.TypeDefinitionInterface _ -> throw500 $ "expected input type, but got output type"
       G.TypeList _ subType -> buildField subType (fieldConstructor' . void . P.list)
  in buildField fieldType fieldConstructor

argumentsParser
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => G.ArgumentsDefinition
  -> G.SchemaDocument
  -> m (InputFieldsParser n ())
argumentsParser args schemaDoc =
  ($> ()) <$> sequenceA <$> traverse (inputValueDefinitionParser schemaDoc) args

-- | 'remoteField' accepts a 'G.TypeDefinition' and will returns a 'FieldParser' for it.
--   Note that the 'G.TypeDefinition' should be of the GraphQL 'Output' kind, when an
--   GraphQL 'Input' kind is provided, then error will be thrown.
remoteField
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
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
    G.TypeDefinitionScalar (G.ScalarTypeDefinition desc name' _) ->
      P.selection fieldName desc argsParser <$> remoteFieldScalarParser name'
    G.TypeDefinitionEnum enumTypeDefn@(G.EnumTypeDefinition desc _ _ _) ->
      pure $ P.selection fieldName desc argsParser $ remoteFieldEnumParser enumTypeDefn
    G.TypeDefinitionInterface ifaceTypeDefn -> do
      remoteSchemaObj <- remoteSchemaInterface sdoc ifaceTypeDefn
      pure $ () <$ P.subselection fieldName (G._itdDescription ifaceTypeDefn) argsParser remoteSchemaObj
    G.TypeDefinitionUnion unionTypeDefn -> do
      remoteSchemaObj <- remoteSchemaUnion sdoc unionTypeDefn
      pure $ () <$ P.subselection fieldName (G._utdDescription unionTypeDefn) argsParser remoteSchemaObj
    _ -> throw500 $ "expected output type, but got input type"

remoteFieldScalarParser
  :: forall m n
   . (MonadParse n, MonadError QErr m)
  => G.Name
  -> m (Parser 'Both n ())
remoteFieldScalarParser name =
  case G.unName name of
    "Boolean" -> pure $ P.boolean $> ()
    "Int" -> pure $ P.int $> ()
    "Float" -> pure $ P.float $> ()
    "String" -> pure $ P.string $> ()
    -- TODO IDs are allowed to be numbers, I think. But not floats. See
    -- http://spec.graphql.org/draft/#sec-ID
    "ID" -> pure $ P.string $> ()
    _ -> pure $ P.unsafeRawScalar name Nothing $> () -- TODO pass correct description

remoteFieldEnumParser
  :: MonadParse n
  => G.EnumTypeDefinition
  -> Parser 'Both n ()
remoteFieldEnumParser (G.EnumTypeDefinition desc name _ valueDefns) =
  let enumValDefns = map (\(G.EnumValueDefinition enumDesc enumName _) ->
                            ((mkDefinition (G.unEnumValue enumName) enumDesc P.EnumValueInfo),()))
                         $ valueDefns
  in P.enum name desc $ NE.fromList enumValDefns
