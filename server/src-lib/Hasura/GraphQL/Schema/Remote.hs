module Hasura.GraphQL.Schema.Remote
  ( buildRemoteParser
  , inputValueDefinitionParser
  , lookupObject
  , lookupType
  , lookupScalar
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashMap.Strict.Extended          as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.List.NonEmpty                    as NE

import           Data.Foldable                         (sequenceA_)
import           Data.Type.Equality
import           Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Data.Text.Extended
import           Hasura.GraphQL.Context                (RemoteField)
import           Hasura.GraphQL.Parser                 as P
import           Hasura.RQL.Types


buildRemoteParser
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => IntrospectionResult
  -> RemoteSchemaInfo
  -> m ( [P.FieldParser n RemoteField]
       , Maybe [P.FieldParser n RemoteField]
       , Maybe [P.FieldParser n RemoteField])
buildRemoteParser (IntrospectionResult sdoc query_root mutation_root subscription_root) info = do
  queryT <- makeParsers query_root
  mutationT <- traverse makeParsers mutation_root
  subscriptionT <- traverse makeParsers subscription_root
  return (queryT, mutationT, subscriptionT)
  where
    makeFieldParser :: G.FieldDefinition -> m (P.FieldParser n RemoteField)
    makeFieldParser fieldDef = do
      fldParser <- remoteField' sdoc fieldDef
      pure $ (info, ) <$> fldParser
    makeParsers :: G.Name -> m [P.FieldParser n RemoteField]
    makeParsers rootName =
      case lookupType sdoc rootName of
        Just (G.TypeDefinitionObject o) ->
          traverse makeFieldParser $ _otdFieldsDefinition o
        _ -> throw400 Unexpected $ rootName <<> " has to be an object type"

remoteField'
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.FieldDefinition
  -> m (FieldParser n (Field NoFragments G.Name))
remoteField' schemaDoc (G.FieldDefinition description name argsDefinition gType _) =
  let
    addNullableList :: FieldParser n (Field NoFragments G.Name) -> FieldParser n (Field NoFragments G.Name)
    addNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser)
      = P.FieldParser (Definition name' un desc (FieldInfo args (Nullable (TList typ)))) parser

    addNonNullableList :: FieldParser n (Field NoFragments G.Name) -> FieldParser n (Field NoFragments G.Name)
    addNonNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser)
      = P.FieldParser (Definition name' un desc (FieldInfo args (NonNullable (TList typ)))) parser

    -- TODO add directives, deprecation
    convertType :: G.GType -> m (FieldParser n (Field NoFragments G.Name))
    convertType gType' = do
        case gType' of
          G.TypeNamed (Nullability True) fieldTypeName ->
            P.nullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (Nullability True) gType'' ->
            addNullableList <$> convertType gType''
          G.TypeNamed (Nullability False) fieldTypeName -> do
            P.nullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (Nullability False) gType'' ->
            addNonNullableList <$> convertType gType''
  in convertType gType

-- | 'remoteSchemaObject' returns a output parser for a given 'ObjectTypeDefinition'.
remoteSchemaObject
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.ObjectTypeDefinition
  -> m (Parser 'Output n [Field NoFragments Name])
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) subFields
  interfaceDefs <- traverse getInterface interfaces
  implements <- traverse (remoteSchemaInterface schemaDoc) interfaceDefs
  -- TODO: also check sub-interfaces, when these are supported in a future graphql spec
  traverse_ validateImplementsFields interfaceDefs
  pure $ P.selectionSetObject name description subFieldParsers implements <&>
    toList . (OMap.mapWithKey $ \alias -> \case
        P.SelectField fld  -> fld
        P.SelectTypename _ ->
          G.Field (Just alias) $$(G.litName "__typename") mempty mempty mempty
    )
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
                  Just a ->
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
        _                   -> False
    validateSubType _ _ = False
    validateSubTypeDefinition x' y' | x' == y' = True
    validateSubTypeDefinition (TypeDefinitionObject otd) (TypeDefinitionInterface itd)
      = G._otdName otd `elem` G._itdPossibleTypes itd
    validateSubTypeDefinition (TypeDefinitionObject _otd) (TypeDefinitionUnion _utd)
      = True -- TODO write appropriate check (may require saving 'possibleTypes' in Syntax.hs)
    validateSubTypeDefinition _ _ = False

-- | helper function to get a parser of an object with it's name
--   This function is called from 'remoteSchemaInterface' and
--   'remoteSchemaObject' functions. Both of these have a slightly
--   different implementation of 'getObject', which is the
--   reason 'getObject' is an argument to this function
getObjectParser
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> (G.Name -> m G.ObjectTypeDefinition)
  -> G.Name
  -> m (Parser 'Output n (Name, [Field NoFragments G.Name]))
getObjectParser schemaDoc getObject objName = do
  obj <- remoteSchemaObject schemaDoc =<< getObject objName
  return $ (objName,) <$> obj

{- Note [Querying remote schema interfaces]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When querying Remote schema interfaces, we need to re-construct
the incoming query to be compliant with the upstream remote.
We need to do this because the `SelectionSet`(s) that are
inputted to this function have the fragments (if any) flattened.
(Check `flattenSelectionSet` in 'Hasura.GraphQL.Parser.Collect' module)
The `constructInterfaceSelectionSet` function makes a valid interface query by:
1. Getting the common interface fields in all the selection sets
2. Remove the common fields obtained in #1 from the selection sets
3. Construct a selection field for every common interface field
4. Construct inline fragments for non-common interface fields
   using the result of #2 for every object
5. Construct the final selection set by combining #3 and #4

Example: Suppose an interface 'Character' is defined in the upstream
and two objects 'Human' and 'Droid' implement the 'Character' Interface.

Suppose, a field 'hero' returns 'Character'.

{
   hero {
     id
     name
     ... on Droid {
       primaryFunction
     }
     ... on Human {
       homePlanet
     }
   }
}

When we parse the selection set of the `hero` field, we parse the selection set
twice: once for the `Droid` object type, which would be passed a selection set
containing the field(s) defined in the `Droid` object type and similarly once
for the 'Human' object type. The result of the interface selection set parsing
would then be the results of the parsing of the object types when passed their
corresponding flattened selection sets and the results of the parsing of the
interface fields.

After we parse the above GraphQL query, we get a selection set containing
the interface fields and the selection sets of the objects that were queried
in the GraphQL query. Since, we have the selection sets of the objects that
were being queried, we can convert them into inline fragments resembling
the original query and then query the remote schema with the newly
constructed query.
-}

-- | 'remoteSchemaInterface' returns a output parser for a given 'InterfaceTypeDefinition'.
--   Also check Note [Querying remote schema interfaces]
remoteSchemaInterface
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.InterfaceTypeDefinition [G.Name]
  -> m (Parser 'Output n (G.SelectionSet NoFragments G.Name))
remoteSchemaInterface schemaDoc defn@(G.InterfaceTypeDefinition description name _directives fields possibleTypes) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) fields
  objs <- traverse (getObjectParser schemaDoc getObject) possibleTypes
  -- In the Draft GraphQL spec (> June 2018), interfaces can themselves
  -- implement superinterfaces.  In the future, we may need to support this
  -- here.
  when (null subFieldParsers) $
    throw400 RemoteSchemaError $ "List of fields cannot be empty for interface " <> squote name
  -- TODO: another way to obtain 'possibleTypes' is to lookup all the object
  -- types in the schema document that claim to implement this interface.  We
  -- should have a check that expresses that that collection of objects is equal
  -- to 'possibleTypes'.
  pure $ P.selectionSetInterface name description subFieldParsers objs <&> constructInterfaceSelectionSet
  where
    getObject :: G.Name -> m G.ObjectTypeDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Interface " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Interface type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

    -- 'constructInterfaceQuery' constructs a remote interface query.
    constructInterfaceSelectionSet
      :: [(G.Name, [Field NoFragments G.Name])]
      -> SelectionSet NoFragments G.Name
    constructInterfaceSelectionSet objNameAndFields =
      let -- common interface fields that exist in every
          -- selection set provided
          -- #1 of Note [Querying remote schema Interfaces]
          commonInterfaceFields =
            Map.elems $
            Map.mapMaybe allTheSame $
            Map.groupOn G._fName $
            concatMap (filter ((`elem` interfaceFieldNames) . G._fName) . snd) $
            objNameAndFields

          interfaceFieldNames = map G._fldName fields

          allTheSame (x:xs) | all (== x) xs = Just x
          allTheSame _ = Nothing

          -- #2 of Note [Querying remote schema interface fields]
          nonCommonInterfaceFields =
            catMaybes $ flip map objNameAndFields $ \(objName, objFields) ->
              let nonCommonFields = filter (not . flip elem commonInterfaceFields) objFields
              in mkObjInlineFragment (objName, map G.SelectionField nonCommonFields)

          -- helper function for #4 of Note [Querying remote schema interface fields]
          mkObjInlineFragment (_, []) = Nothing
          mkObjInlineFragment (objName, selSet) =
            Just $ G.SelectionInlineFragment $
              G.InlineFragment (Just objName) mempty selSet

      -- #5 of Note [Querying remote schema interface fields]
      in (fmap G.SelectionField commonInterfaceFields) <> nonCommonInterfaceFields

-- | 'remoteSchemaUnion' returns a output parser for a given 'UnionTypeDefinition'.
remoteSchemaUnion
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.UnionTypeDefinition
  -> m (Parser 'Output n (SelectionSet NoFragments G.Name))
remoteSchemaUnion schemaDoc defn@(G.UnionTypeDefinition description name _directives objectNames) =
  P.memoizeOn 'remoteSchemaObject defn do
  objs <- traverse (getObjectParser schemaDoc getObject) objectNames
  when (null objs) $
    throw400 RemoteSchemaError $ "List of member types cannot be empty for union type " <> squote name
  pure $ P.selectionSetUnion name description objs <&>
    (\objNameAndFields ->
       catMaybes $ objNameAndFields <&> \(objName, fields) ->
        case fields of
          -- The return value obtained from the parsing of a union selection set
          -- specifies, for each object in the union type, a fragment-free
          -- selection set for that object type. In particular, if, for a given
          -- object type, the selection set passed to the union type did not
          -- specify any fields for that object type (i.e. if no inline fragment
          -- applied to that object), the selection set resulting from the parsing
          -- through that object type would be empty, i.e. []. We exclude such
          -- object types from the reconstructed selection set for the union
          -- type, as selection sets cannot be empty.
          [] -> Nothing
          _  ->
            Just (G.SelectionInlineFragment
                    $ G.InlineFragment (Just objName) mempty $ fmap G.SelectionField fields))
  where
    getObject :: G.Name -> m G.ObjectTypeDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
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
  -> Maybe G.Description
  -> G.Name
  -> G.ArgumentsDefinition
  -> m (FieldParser n (Field NoFragments G.Name))
remoteFieldFromName sdoc fieldName description fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing      -> throw400 RemoteSchemaError $ "Could not find type with name " <>> fieldName
    Just typeDef -> remoteField sdoc fieldName description argsDefns typeDef

-- | 'inputValuefinitionParser' accepts a 'G.InputValueDefinition' and will return an
--   'InputFieldsParser' for it. If a non 'Input' GraphQL type is found in the 'type' of
--    the 'InputValueDefinition' then an error will be thrown.
inputValueDefinitionParser
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => G.SchemaIntrospection
  -> G.InputValueDefinition
  -> m (InputFieldsParser n (Maybe (InputValue Variable)))
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal) =
  let fieldConstructor :: forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Maybe (InputValue Variable))
      fieldConstructor parser =
        let wrappedParser :: Parser k n (InputValue Variable)
            wrappedParser =
              P.Parser
                { P.pType   = P.pType parser
                , P.pParser = \value -> P.pParser parser value $> castWith (P.inputParserInput @k) value
                }
        in case maybeDefaultVal of
          Nothing ->
            if G.isNullable fieldType
            then fieldOptional name desc wrappedParser
            else Just <$> field name desc wrappedParser
          Just defaultVal -> Just <$> fieldWithDefault name desc defaultVal wrappedParser
      doNullability :: forall k . 'Input <: k => G.Nullability -> Parser k n () -> Parser k n ()
      doNullability (G.Nullability True)  = void . P.nullable
      doNullability (G.Nullability False) = id
      buildField
        :: G.GType
        -> (forall k. 'Input <: k => Parser k n () -> InputFieldsParser n (Maybe (InputValue Variable)))
        -> m (InputFieldsParser n (Maybe (InputValue Variable)))
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed nullability typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <>> typeName
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar scalarTypeDefn ->
                 pure $ fieldConstructor' $ doNullability nullability $ remoteFieldScalarParser scalarTypeDefn
               G.TypeDefinitionEnum defn ->
                 pure $ fieldConstructor' $ doNullability nullability $ remoteFieldEnumParser defn
               G.TypeDefinitionObject _ -> throw400 RemoteSchemaError "expected input type, but got output type" -- couldn't find the equivalent error in Validate/Types.hs, so using a new error message
               G.TypeDefinitionInputObject defn ->
                 fieldConstructor' . doNullability nullability <$> remoteSchemaInputObject schemaDoc defn
               G.TypeDefinitionUnion _ -> throw400 RemoteSchemaError "expected input type, but got output type"
               G.TypeDefinitionInterface _ -> throw400 RemoteSchemaError "expected input type, but got output type"
       G.TypeList nullability subType -> buildField subType (fieldConstructor' . doNullability nullability . void . P.list)
  in buildField fieldType fieldConstructor

argumentsParser
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => G.ArgumentsDefinition
  -> G.SchemaIntrospection
  -> m (InputFieldsParser n ())
argumentsParser args schemaDoc = do
  sequenceA_ <$> for args (inputValueDefinitionParser schemaDoc)

-- | 'remoteField' accepts a 'G.TypeDefinition' and will returns a 'FieldParser' for it.
--   Note that the 'G.TypeDefinition' should be of the GraphQL 'Output' kind, when an
--   GraphQL 'Input' kind is provided, then error will be thrown.
remoteField
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => SchemaIntrospection
  -> G.Name
  -> Maybe G.Description
  -> G.ArgumentsDefinition
  -> G.TypeDefinition [G.Name]
  -> m (FieldParser n (Field NoFragments G.Name))
remoteField sdoc fieldName description argsDefn typeDefn = do
  -- TODO add directives
  argsParser <- argumentsParser argsDefn sdoc
  case typeDefn of
    G.TypeDefinitionObject objTypeDefn -> do
      remoteSchemaObjFields <- remoteSchemaObject sdoc objTypeDefn
      -- converting [Field NoFragments Name] to (SelectionSet NoFragments G.Name)
      let remoteSchemaObjSelSet = fmap G.SelectionField <$> remoteSchemaObjFields
      pure remoteSchemaObjSelSet
        <&> mkFieldParserWithSelectionSet argsParser
    G.TypeDefinitionScalar scalarTypeDefn ->
      pure $ mkFieldParserWithoutSelectionSet argsParser
             $ remoteFieldScalarParser scalarTypeDefn
    G.TypeDefinitionEnum enumTypeDefn ->
      pure $ mkFieldParserWithoutSelectionSet argsParser
             $ remoteFieldEnumParser enumTypeDefn
    G.TypeDefinitionInterface ifaceTypeDefn ->
      remoteSchemaInterface sdoc ifaceTypeDefn <&>
        mkFieldParserWithSelectionSet argsParser
    G.TypeDefinitionUnion unionTypeDefn ->
      remoteSchemaUnion sdoc unionTypeDefn <&>
        mkFieldParserWithSelectionSet argsParser
    _ -> throw400 RemoteSchemaError "expected output type, but got input type"
  where
    mkFieldParserWithoutSelectionSet
      :: InputFieldsParser n ()
      -> Parser 'Both n ()
      -> FieldParser n (Field NoFragments G.Name)
    mkFieldParserWithoutSelectionSet argsParser outputParser =
      -- 'rawSelection' is used here to get the alias and args data
      -- specified to be able to construct the `Field NoFragments G.Name`
      P.rawSelection fieldName description argsParser outputParser
      <&> (\(alias, args, _) -> (G.Field alias fieldName (fmap getName <$> args) mempty []))

    mkFieldParserWithSelectionSet
      :: InputFieldsParser n ()
      -> Parser 'Output n (SelectionSet NoFragments G.Name)
      -> FieldParser n (Field NoFragments G.Name)
    mkFieldParserWithSelectionSet argsParser outputParser =
      -- 'rawSubselection' is used here to get the alias and args data
      -- specified to be able to construct the `Field NoFragments G.Name`
      P.rawSubselection fieldName description argsParser outputParser
      <&> (\(alias, args, _, selSet) ->
             (G.Field alias fieldName (fmap getName <$> args) mempty selSet))

remoteFieldScalarParser
  :: MonadParse n
  => G.ScalarTypeDefinition
  -> Parser 'Both n ()
remoteFieldScalarParser (G.ScalarTypeDefinition description name _directives) =
  case G.unName name of
    "Boolean" -> P.boolean $> ()
    "Int"     -> P.int $> ()
    "Float"   -> P.float $> ()
    "String"  -> P.string $> ()
    "ID"      -> P.identifier $> ()
    _         -> P.unsafeRawScalar name description $> ()

remoteFieldEnumParser
  :: MonadParse n
  => G.EnumTypeDefinition
  -> Parser 'Both n ()
remoteFieldEnumParser (G.EnumTypeDefinition desc name _directives valueDefns) =
  let enumValDefns = valueDefns <&> \(G.EnumValueDefinition enumDesc enumName _) ->
        (mkDefinition (G.unEnumValue enumName) enumDesc P.EnumValueInfo,())
  in P.enum name desc $ NE.fromList enumValDefns
