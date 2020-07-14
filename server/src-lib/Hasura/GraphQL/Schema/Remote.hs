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
import           Hasura.SQL.Types

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
buildRemoteParser (RemoteSchemaCtx _name (sdoc, query_root, mutation_root, subscription_root) info _) = do
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
        _ -> throw400 RemoteSchemaError $ "Root type of unexpected type" -- TODO show rootName

-- | 'remoteFieldFullSchema' takes the 'SchemaIntrospection' and a 'G.Name' and will
--   return a 'SelectionSet' parser if the 'G.Name' is found and is a 'TypeDefinitionObject',
--   otherwise, an error will be thrown.
remoteFieldFullSchema
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.Name
  -> m (Parser 'Output n (G.SelectionSet NoFragments Variable))
remoteFieldFullSchema sdoc name =
  P.memoizeOn 'remoteFieldFullSchema name do
  fieldObjectType <-
    case lookupType sdoc name of
      Just (G.TypeDefinitionObject o) -> pure o
      _ -> throw400 RemoteSchemaError $ "object with " <> G.unName name <> " not found"
  fieldParser <- remoteSchemaObject sdoc fieldObjectType
  pure $ P.unsafeRawParser (P.pType fieldParser)

remoteField'
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
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
  => SchemaIntrospection
  -> G.ObjectTypeDefinition
  -> m (Parser 'Output n ())
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) subFields
  interfaceDefs <- traverse getInterface interfaces
  implements <- traverse (remoteSchemaInterface schemaDoc) interfaceDefs
  -- TODO: also check sub-interfaces, when these are supported in a future graphql spec
  traverse validateImplementsFields interfaceDefs
  pure $ () <$ P.selectionSetObject name description subFieldParsers implements
  where
    getInterface :: G.Name -> m (G.InterfaceTypeDefinition [G.Name])
    getInterface interfaceName =
      onNothing (lookupInterface schemaDoc interfaceName) $
        throw400 RemoteSchemaError $ "Could not find interface " <> squote interfaceName
        <> " implemented by Object type " <> squote name
    validateImplementsFields :: G.InterfaceTypeDefinition [G.Name] -> m ()
    validateImplementsFields interface =
      traverse_ (validateImplementsField (_itdName interface)) (G._itdFieldsDefinition interface)
    validateImplementsField :: G.Name -> G.FieldDefinition -> m ()
    validateImplementsField interfaceName interfaceField =
      case lookup (G._fldName interfaceField) (zip (fmap G._fldName subFields) subFields) of
        Nothing -> throw400 RemoteSchemaError $
          "Interface field " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
          <> " expected, but " <> squote name <> " does not provide it"
        Just f -> do
          unless (validateSubType (G._fldType f) (G._fldType interfaceField)) $
            throw400 RemoteSchemaError $
            "The type of Object field " <> squote name <> "." <> dquote (G._fldName f)
            <> " (" <> G.showGT (G._fldType f)
            <> ") is not the same type/sub type of Interface field "
            <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
            <> " (" <> G.showGT (G._fldType interfaceField) <> ")"
          traverse_ (validateArgument (G._fldArgumentsDefinition f)) (G._fldArgumentsDefinition interfaceField)
          traverse_ (validateNoExtraNonNull (G._fldArgumentsDefinition interfaceField)) (G._fldArgumentsDefinition f)
            where
              validateArgument :: G.ArgumentsDefinition -> G.InputValueDefinition -> m ()
              validateArgument objectFieldArgs ifaceArgument =
                case lookup (G._ivdName ifaceArgument) (zip (fmap G._ivdName objectFieldArgs) objectFieldArgs) of
                  Nothing ->
                    throw400 RemoteSchemaError $
                      "Interface field argument " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
                      <> "(" <> dquote (G._ivdName ifaceArgument) <> ":) required, but Object field " <> squote name <> "." <> dquote (G._fldName f)
                      <> " does not provide it"
                  Just a -> do
                    unless (G._ivdType a == G._ivdType ifaceArgument) $
                      throw400 RemoteSchemaError $
                      "Interface field argument " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
                      <> "(" <> dquote (G._ivdName ifaceArgument) <> ":) expects type "
                      <> G.showGT (G._ivdType ifaceArgument)
                      <> ", but " <> squote name <> "." <> dquote (G._fldName f) <> "("
                      <> dquote (G._ivdName ifaceArgument) <> ":) has type "
                      <> G.showGT (G._ivdType a)
              validateNoExtraNonNull :: G.ArgumentsDefinition -> G.InputValueDefinition -> m ()
              validateNoExtraNonNull ifaceArguments objectFieldArg =
                case lookup (G._ivdName objectFieldArg) (zip (fmap G._ivdName ifaceArguments) ifaceArguments) of
                  Just _ -> pure ()
                  Nothing ->
                    unless (G.isNullable (G._ivdType objectFieldArg)) $
                    throw400 RemoteSchemaError $
                    "Object field argument " <>  squote name <> "." <> dquote (G._fldName f) <> "("
                      <> dquote (G._ivdName objectFieldArg) <> ":) is of required type "
                      <> G.showGT (G._ivdType objectFieldArg) <> ", but is not provided by Interface field "
                      <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
    validateSubType :: G.GType -> G.GType -> Bool
    -- TODO this ignores nullability which is probably wrong, even though the GraphQL spec is ambiguous
    validateSubType (G.TypeList _ x) (G.TypeList _ y) = validateSubType x y
    -- It is OK to "upgrade" the strictness
    validateSubType (G.TypeNamed (Nullability False) x) (G.TypeNamed (Nullability True) y) =
      validateSubType (G.TypeNamed (Nullability True) x) (G.TypeNamed (Nullability True) y)
    validateSubType (G.TypeNamed nx x) (G.TypeNamed ny y) =
      case (lookupType schemaDoc x , lookupType schemaDoc y) of
        (Just x' , Just y') -> nx == ny && validateSubTypeDefinition x' y'
        _ -> False
    validateSubType _ _ = False
    validateSubTypeDefinition x' y' | x' == y' = True
    validateSubTypeDefinition (TypeDefinitionObject otd) (TypeDefinitionInterface itd)
      = G._otdName otd `elem` G._itdPossibleTypes itd
    validateSubTypeDefinition (TypeDefinitionObject _otd) (TypeDefinitionUnion _utd)
      = True -- TODO write appropriate check (may require saving 'possibleTypes' in Syntax.hs)
    validateSubTypeDefinition _ _ = False

objectsImplementingInterface
  :: SchemaIntrospection
  -> G.Name
  -> [G.ObjectTypeDefinition]
objectsImplementingInterface (SchemaIntrospection tps) interfaceName = catMaybes $ fmap go tps
  where
    go :: TypeDefinition possibleTypes -> Maybe G.ObjectTypeDefinition
    go (TypeDefinitionObject obj@(G.ObjectTypeDefinition _desc _name interfaces _directives _fields))
      | interfaceName `elem` interfaces = Just obj
    go _ = Nothing

-- In the Draft GraphQL spec, interfaces can themselves implement
-- superinterfaces.  In the future, we may need to support this.  Currently,
-- this function always returns false.
interfaceImplementingInterface
  :: SchemaIntrospection
  -> G.Name
  -> [G.InterfaceTypeDefinition [G.Name]]
interfaceImplementingInterface (SchemaIntrospection tps) interfaceName = catMaybes $ fmap go tps
  where
    go :: TypeDefinition possibleTypes -> Maybe (G.InterfaceTypeDefinition possibleTypes)
    go (TypeDefinitionInterface iface@(G.InterfaceTypeDefinition _desc _name _directives _fields _possibleTypes))
      -- TODO in order to support interfaces implementing interfaces, fill in
      -- the right value for [] here.
      | interfaceName `elem` [] = Just iface
    go _ = Nothing

-- | 'remoteSchemaInterface' returns a output parser for a given 'InterfaceTypeDefinition'.
remoteSchemaInterface
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.InterfaceTypeDefinition [G.Name]
  -> m (Parser 'Output n ())
remoteSchemaInterface schemaDoc defn@(G.InterfaceTypeDefinition description name _directives fields possibleTypes) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) fields
  objs :: [Parser 'Output n ()] <-
    traverse (\objName -> getObject objName >>= remoteSchemaObject schemaDoc) $ possibleTypes
  ifaces :: [Parser 'Output n ()] <- traverse (remoteSchemaInterface schemaDoc) $
    interfaceImplementingInterface schemaDoc name
  when (null ifaces && null subFieldParsers) $
    throw400 RemoteSchemaError $ "List of fields cannot be empty for interface " <> squote name
  -- TODO: another way to obtain 'possibleTypes' is to lookup all the object
  -- types in the schema document that claim to implement this interface.  We
  -- should have a check that expresses that that collection of objects is equal
  -- to 'possibelTypes'.
  pure $ () <$ (P.selectionSetInterface name description subFieldParsers objs ifaces)
  where
    getObject :: G.Name -> m G.ObjectTypeDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case (lookupInterface schemaDoc objectName) of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Union " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Union type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

-- | 'remoteSchemaUnion' returns a output parser for a given 'UnionTypeDefinition'.
remoteSchemaUnion
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.UnionTypeDefinition
  -> m (Parser 'Output n ())
remoteSchemaUnion schemaDoc defn@(G.UnionTypeDefinition description name _directives objectNames) =
  P.memoizeOn 'remoteSchemaObject defn do
  objDefs <- traverse getObject objectNames
  objs :: [Parser 'Output n ()] <- traverse (remoteSchemaObject schemaDoc) $ objDefs
  when (null objs) $
    throw400 RemoteSchemaError $ "List of member types cannot be empty for union type " <> squote name
  pure $ () <$ (P.selectionSetUnion name description objs)
  where
    getObject :: G.Name -> m G.ObjectTypeDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case (lookupInterface schemaDoc objectName) of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Union " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Union type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

-- | remoteSchemaInputObject returns an input parser for a given 'G.InputObjectTypeDefinition'
remoteSchemaInputObject
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.InputObjectTypeDefinition
  -> m (Parser 'Input n ())
remoteSchemaInputObject schemaDoc defn@(G.InputObjectTypeDefinition desc name _ valueDefns) =
  P.memoizeOn 'remoteSchemaInputObject defn do
  argsParser <- argumentsParser valueDefns schemaDoc
  pure $ P.object name desc argsParser

lookupType :: SchemaIntrospection -> G.Name -> Maybe (G.TypeDefinition [G.Name])
lookupType (SchemaIntrospection types) name = find (\tp -> getNamedTyp tp == name) types
  where
    getNamedTyp :: G.TypeDefinition possibleTypes -> G.Name
    getNamedTyp ty = case ty of
      G.TypeDefinitionScalar t      -> G._stdName t
      G.TypeDefinitionObject t      -> G._otdName t
      G.TypeDefinitionInterface t   -> G._itdName t
      G.TypeDefinitionUnion t       -> G._utdName t
      G.TypeDefinitionEnum t        -> G._etdName t
      G.TypeDefinitionInputObject t -> G._iotdName t

lookupObject :: SchemaIntrospection -> G.Name -> Maybe G.ObjectTypeDefinition
lookupObject (SchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes] -> Maybe G.ObjectTypeDefinition
    go ((G.TypeDefinitionObject t):tps)
      | G._otdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupInterface :: SchemaIntrospection -> G.Name -> Maybe (G.InterfaceTypeDefinition [G.Name])
lookupInterface (SchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes] -> Maybe (G.InterfaceTypeDefinition possibleTypes)
    go ((G.TypeDefinitionInterface t):tps)
      | G._itdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupScalar :: SchemaIntrospection -> G.Name -> Maybe G.ScalarTypeDefinition
lookupScalar (SchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes] -> Maybe G.ScalarTypeDefinition
    go ((G.TypeDefinitionScalar t):tps)
      | G._stdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

-- | 'remoteFieldFromName' accepts a GraphQL name and searches for its definition
--   in the 'SchemaIntrospection'.
remoteFieldFromName
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.Name
  -> G.Name
  -> G.ArgumentsDefinition
  -> m (FieldParser n ())
remoteFieldFromName sdoc fieldName fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <> G.unName fieldName
    Just typeDef -> remoteField sdoc fieldName argsDefns typeDef

-- | 'inputValuefinitionParser' accepts a 'G.InputValueDefinition' and will return an
--   'InputFieldsParser' for it. If a non 'Input' GraphQL type is found in the 'type' of
--    the 'InputValueDefinition' then an error will be thrown.
inputValueDefinitionParser
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => G.SchemaIntrospection
  -> G.InputValueDefinition
  -> m (InputFieldsParser n (Maybe (G.Value Variable)))
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal) =
  let fieldConstructor :: forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Maybe (Value Variable))
      fieldConstructor parser =
        let wrappedParser :: Parser k n (Value Variable)
            wrappedParser =
              P.Parser
                { P.pType   = P.pType parser
                , P.pParser = \value -> P.pParser parser value $> castWith (P.inputParserInput @k) value
                }
        in case maybeDefaultVal of
          Nothing ->
            case G.isNullable fieldType of
              True -> fieldOptional name desc wrappedParser
              False -> fmap Just $ field name desc wrappedParser
          Just defaultVal -> fmap Just $ fieldWithDefault name desc defaultVal wrappedParser
      doNullability :: forall k . 'Input <: k => G.Nullability -> Parser k n () -> Parser k n ()
      doNullability (G.Nullability True)  = void . P.nullable
      doNullability (G.Nullability False) = id
      buildField
        :: G.GType
        -> (forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Maybe (G.Value Variable)))
        -> m (InputFieldsParser n (Maybe (G.Value Variable)))
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed nullability typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <> G.unName typeName
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar (G.ScalarTypeDefinition _ name' _) ->
                 fieldConstructor' . doNullability nullability <$> remoteFieldScalarParser name'
               G.TypeDefinitionEnum defn -> pure $ fieldConstructor' $ remoteFieldEnumParser defn
               G.TypeDefinitionObject _ -> throw400 RemoteSchemaError $ "expected input type, but got output type" -- couldn't find the equivalent error in Validate/Types.hs, so using a new error message
               G.TypeDefinitionInputObject defn ->
                 pure . fieldConstructor' . doNullability nullability =<< remoteSchemaInputObject schemaDoc defn
               G.TypeDefinitionUnion _ -> throw400 RemoteSchemaError $ "expected input type, but got output type"
               G.TypeDefinitionInterface _ -> throw400 RemoteSchemaError $ "expected input type, but got output type"
       G.TypeList nullability subType -> buildField subType (fieldConstructor' . doNullability nullability . void . P.list)
  in buildField fieldType fieldConstructor

argumentsParser
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => G.ArgumentsDefinition
  -> G.SchemaIntrospection
  -> m (InputFieldsParser n ())
argumentsParser args schemaDoc =
  ($> ()) <$> sequenceA <$> traverse (inputValueDefinitionParser schemaDoc) args

-- | 'remoteField' accepts a 'G.TypeDefinition' and will returns a 'FieldParser' for it.
--   Note that the 'G.TypeDefinition' should be of the GraphQL 'Output' kind, when an
--   GraphQL 'Input' kind is provided, then error will be thrown.
remoteField
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.Name
  -> G.ArgumentsDefinition
  -> G.TypeDefinition [G.Name]
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
    _ -> throw400 RemoteSchemaError $ "expected output type, but got input type"

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
