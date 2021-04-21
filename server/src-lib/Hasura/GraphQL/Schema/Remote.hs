module Hasura.GraphQL.Schema.Remote
  ( buildRemoteParser
  , inputValueDefinitionParser
  , lookupObject
  , lookupType
  , lookupScalar
  , remoteField
  , lookupInterface
  , lookupUnion
  , lookupEnum
  , lookupInputObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                   as Map
import qualified Data.HashMap.Strict.InsOrd            as OMap
import qualified Data.HashMap.Strict.InsOrd.Extended   as OMap
import qualified Data.List.NonEmpty                    as NE

import           Data.Text.Extended
import           Data.Type.Equality
import           Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.GraphQL.Context                (RemoteField, RemoteFieldG (..))
import           Hasura.GraphQL.Parser                 as P
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache


type RemoteSchemaObjectDefinition      = G.ObjectTypeDefinition RemoteSchemaInputValueDefinition
type RemoteSchemaInputObjectDefinition = G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition
type RemoteSchemaInterfaceDefinition   = G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition
type RemoteSchemaFieldDefinition       = G.FieldDefinition RemoteSchemaInputValueDefinition
type RemoteSchemaTypeDefinition        = G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition

buildRemoteParser
  :: forall m n
   . (MonadSchema n m, MonadError QErr m)
  => IntrospectionResult
  -> RemoteSchemaInfo
  -> m ( [P.FieldParser n RemoteField]
       , Maybe [P.FieldParser n RemoteField]
       , Maybe [P.FieldParser n RemoteField])
buildRemoteParser (IntrospectionResult sdoc queryRoot mutationRoot subscriptionRoot) info = do
  queryT <- makeParsers queryRoot
  mutationT <- makeNonQueryRootFieldParser mutationRoot $$(G.litName "Mutation")
  subscriptionT <- makeNonQueryRootFieldParser subscriptionRoot $$(G.litName "Subscription")
  return (queryT, mutationT, subscriptionT)
  where
    makeFieldParser :: RemoteSchemaFieldDefinition -> m (P.FieldParser n RemoteField)
    makeFieldParser fieldDef = do
      fldParser <- remoteField' sdoc fieldDef
      pure $ (RemoteFieldG info) <$> fldParser
    makeParsers :: G.Name -> m [P.FieldParser n RemoteField]
    makeParsers rootName =
      case lookupType sdoc rootName of
        Just (G.TypeDefinitionObject o) ->
          traverse makeFieldParser $ _otdFieldsDefinition o
        _ -> throw400 Unexpected $ rootName <<> " has to be an object type"

    -- | The spec says that the `schema` definition can be omitted, if the root names
    --   are the defaults (Query, Mutation and Subscription). This function is used
    --   to constructor a `FieldParser` for the mutation and subscription roots.
    --   If the user has given a custom Mutation/Subscription root name, then it will
    --   look for that and if it's not found in the schema document, then an error is thrown.
    --   If no root name has been provided, we lookup the schema document for an object with
    --   the default name and if that's not found, we omit the said Root from the schema.
    makeNonQueryRootFieldParser :: Maybe G.Name -> G.Name -> m (Maybe [P.FieldParser n RemoteField])
    makeNonQueryRootFieldParser userProvidedRootName defaultRootName =
      case userProvidedRootName of
        Just _rootName -> traverse makeParsers userProvidedRootName
        Nothing ->
          let isDefaultRootObjectExists = isJust $ lookupObject sdoc defaultRootName
          in bool (pure Nothing) (traverse makeParsers $ Just defaultRootName) $ isDefaultRootObjectExists

remoteField'
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> RemoteSchemaFieldDefinition
  -> m (FieldParser n (Field NoFragments RemoteSchemaVariable))
remoteField' schemaDoc (G.FieldDefinition description name argsDefinition gType _) =
  let
    addNullableList :: FieldParser n (Field NoFragments RemoteSchemaVariable) -> FieldParser n (Field NoFragments RemoteSchemaVariable)
    addNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser)
      = P.FieldParser (Definition name' un desc (FieldInfo args (Nullable (TList typ)))) parser

    addNonNullableList :: FieldParser n (Field NoFragments RemoteSchemaVariable) -> FieldParser n (Field NoFragments RemoteSchemaVariable)
    addNonNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser)
      = P.FieldParser (Definition name' un desc (FieldInfo args (NonNullable (TList typ)))) parser

    -- TODO add directives, deprecation
    convertType :: G.GType -> m (FieldParser n (Field NoFragments RemoteSchemaVariable))
    convertType gType' = do
        case gType' of
          G.TypeNamed (Nullability True) fieldTypeName ->
            P.nullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (Nullability True) gType'' ->
            addNullableList <$> convertType gType''
          G.TypeNamed (Nullability False) fieldTypeName -> do
            P.nonNullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (Nullability False) gType'' ->
            addNonNullableList <$> convertType gType''
  in convertType gType

-- | 'remoteSchemaObject' returns a output parser for a given 'ObjectTypeDefinition'.
remoteSchemaObject
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> G.ObjectTypeDefinition RemoteSchemaInputValueDefinition
  -> m (Parser 'Output n [Field NoFragments RemoteSchemaVariable])
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
  subFieldParsers <- traverse (remoteField' schemaDoc) subFields
  interfaceDefs <- traverse getInterface interfaces
  implements <- traverse (remoteSchemaInterface schemaDoc) interfaceDefs
  -- TODO: also check sub-interfaces, when these are supported in a future graphql spec
  traverse_ validateImplementsFields interfaceDefs
  pure $ P.selectionSetObject name description subFieldParsers implements <&>
    toList . OMap.mapWithKey (\alias -> \case
        P.SelectField fld  -> fld
        P.SelectTypename _ ->
          G.Field (Just alias) $$(G.litName "__typename") mempty mempty mempty)
  where
    getInterface :: G.Name -> m RemoteSchemaInterfaceDefinition
    getInterface interfaceName =
      onNothing (lookupInterface schemaDoc interfaceName) $
        throw400 RemoteSchemaError $ "Could not find interface " <> squote interfaceName
        <> " implemented by Object type " <> squote name
    validateImplementsFields :: RemoteSchemaInterfaceDefinition -> m ()
    validateImplementsFields interface =
      traverse_ (validateImplementsField (_itdName interface)) (G._itdFieldsDefinition interface)
    validateImplementsField :: G.Name -> RemoteSchemaFieldDefinition -> m ()
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
          traverse_
            (validateArgument
               (map _rsitdDefinition (G._fldArgumentsDefinition f)) . _rsitdDefinition)
            (G._fldArgumentsDefinition interfaceField)
          traverse_
            (validateNoExtraNonNull
               (map _rsitdDefinition (G._fldArgumentsDefinition interfaceField)) . _rsitdDefinition)
            (G._fldArgumentsDefinition f)
            where
              validateArgument :: [G.InputValueDefinition] -> G.InputValueDefinition -> m ()
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
              validateNoExtraNonNull :: [G.InputValueDefinition] -> G.InputValueDefinition -> m ()
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
  => RemoteSchemaIntrospection
  -> (G.Name -> m RemoteSchemaObjectDefinition)
  -> G.Name
  -> m (Parser 'Output n (Name, [Field NoFragments RemoteSchemaVariable]))
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
  => RemoteSchemaIntrospection
  -> RemoteSchemaInterfaceDefinition
  -> m (Parser 'Output n (G.SelectionSet NoFragments RemoteSchemaVariable))
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
    getObject :: G.Name -> m RemoteSchemaObjectDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Interface " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Interface type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

    -- 'constructInterfaceQuery' constructs a remote interface query.
    constructInterfaceSelectionSet
      :: [(G.Name, [Field NoFragments RemoteSchemaVariable])]
      -> SelectionSet NoFragments RemoteSchemaVariable
    constructInterfaceSelectionSet objNameAndFields =
      let -- common interface fields that exist in every
          -- selection set provided
          -- #1 of Note [Querying remote schema Interfaces]
          commonInterfaceFields =
            OMap.elems $
            OMap.mapMaybe (allTheSame . toList) $
            OMap.groupListWith G._fName $
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
      in fmap G.SelectionField commonInterfaceFields <> nonCommonInterfaceFields

-- | 'remoteSchemaUnion' returns a output parser for a given 'UnionTypeDefinition'.
remoteSchemaUnion
  :: forall n m
  . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> G.UnionTypeDefinition
  -> m (Parser 'Output n (SelectionSet NoFragments RemoteSchemaVariable))
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
    getObject :: G.Name -> m RemoteSchemaObjectDefinition
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type " <> squote objectName
            <> ", which is defined as a member type of Union " <> squote name
          Just _  -> throw400 RemoteSchemaError $ "Union type " <> squote name <>
            " can only include object types. It cannot include " <> squote objectName

-- | remoteSchemaInputObject returns an input parser for a given 'G.InputObjectTypeDefinition'
--
-- Now, this is tricky! We are faced with two contradicting constraints here. On one hand, the
-- GraphQL spec forbids us from creating empty input objects. This means that if all the arguments
-- have presets, we CANNOT use the parser this function creates, and the caller cannot create a
-- field for this object (and instead should use @pure@ to include the preset values in the result
-- of parsing the fields).
--
-- One way we could fix this would be to change the type of this function to return a `Maybe
-- Parser`, inspect the result of 'argumentsParser', and return @Nothing@ when we realize that there
-- aren't any actual field in it (or at least return a value that propagates the preset values). But
-- this would contradict our second constraint: this function needs to be memoized!
--
-- At time of writing, we can't memoize functions that return arbitrary functors of Parsers; so no
-- memoizing Maybe Parser or Either Presets Parser. Which means that we would need to first call
-- `argumentsParser`, then memoize the "Just" branch that builds the actual Parser. The problem is
-- that the recursive call ro remoteSchemaInputObject is within 'argumentsParser', meaning the call
-- to it MUST be in the memoized branch!
--
-- This is why, in the end, we do the following: we first test whether there is any non-preset
-- field: if yes, we memoize that branch and proceed as normal. Otherwise we can omit the
-- memoization: we know for sure that the preset fields won't generate a recursive call!
remoteSchemaInputObject
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition
  -> m ( Either
           (InputFieldsParser n (Maybe (HashMap G.Name (Value RemoteSchemaVariable))))
           (Parser 'Input     n (Maybe (HashMap G.Name (Value RemoteSchemaVariable))))
       )
remoteSchemaInputObject schemaDoc defn@(G.InputObjectTypeDefinition desc name _ valueDefns) =
  if all (isJust . _rsitdPresetArgument) valueDefns
  then
    -- All the fields are preset: we can't create a parser, that would result in an invalid type in
    -- the schema (an input object with no field). We therefore forward the InputFieldsParser
    -- unmodified. No need to memoize this branch: since all arguments are preset, 'argumentsParser'
    -- won't be recursively calling this function.
    Left <$> argumentsParser valueDefns schemaDoc
  else
    -- At least one field is not a preset, meaning we have the guarantee that there will be at least
    -- one field in the input object. We have to memoize this branch as we might recursively call
    -- the same parser.
    Right <$> P.memoizeOn 'remoteSchemaInputObject defn do
      argsParser <- argumentsParser valueDefns schemaDoc
      pure $ P.object name desc $ argsParser

lookupType
  :: RemoteSchemaIntrospection
  -> G.Name
  -> Maybe RemoteSchemaTypeDefinition
lookupType (RemoteSchemaIntrospection types) name = find (\tp -> getNamedTyp tp == name) types
  where
    getNamedTyp :: G.TypeDefinition possibleTypes RemoteSchemaInputValueDefinition -> G.Name
    getNamedTyp ty = case ty of
      G.TypeDefinitionScalar t      -> G._stdName t
      G.TypeDefinitionObject t      -> G._otdName t
      G.TypeDefinitionInterface t   -> G._itdName t
      G.TypeDefinitionUnion t       -> G._utdName t
      G.TypeDefinitionEnum t        -> G._etdName t
      G.TypeDefinitionInputObject t -> G._iotdName t

lookupObject :: RemoteSchemaIntrospection -> G.Name -> Maybe RemoteSchemaObjectDefinition
lookupObject (RemoteSchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes  RemoteSchemaInputValueDefinition] -> Maybe RemoteSchemaObjectDefinition
    go ((G.TypeDefinitionObject t):tps)
      | G._otdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupInterface :: RemoteSchemaIntrospection -> G.Name -> Maybe RemoteSchemaInterfaceDefinition
lookupInterface (RemoteSchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes RemoteSchemaInputValueDefinition]
        -> Maybe (G.InterfaceTypeDefinition possibleTypes RemoteSchemaInputValueDefinition)
    go ((G.TypeDefinitionInterface t):tps)
      | G._itdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupScalar :: RemoteSchemaIntrospection -> G.Name -> Maybe G.ScalarTypeDefinition
lookupScalar (RemoteSchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes RemoteSchemaInputValueDefinition] -> Maybe G.ScalarTypeDefinition
    go ((G.TypeDefinitionScalar t):tps)
      | G._stdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupUnion :: RemoteSchemaIntrospection -> G.Name -> Maybe G.UnionTypeDefinition
lookupUnion (RemoteSchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes RemoteSchemaInputValueDefinition] -> Maybe G.UnionTypeDefinition
    go ((G.TypeDefinitionUnion t):tps)
      | G._utdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupEnum :: RemoteSchemaIntrospection -> G.Name -> Maybe G.EnumTypeDefinition
lookupEnum (RemoteSchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes RemoteSchemaInputValueDefinition] -> Maybe G.EnumTypeDefinition
    go ((G.TypeDefinitionEnum t):tps)
      | G._etdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

lookupInputObject :: RemoteSchemaIntrospection -> G.Name -> Maybe RemoteSchemaInputObjectDefinition
lookupInputObject (RemoteSchemaIntrospection types) name = go types
  where
    go :: [TypeDefinition possibleTypes RemoteSchemaInputValueDefinition] -> Maybe RemoteSchemaInputObjectDefinition
    go ((G.TypeDefinitionInputObject t):tps)
      | G._iotdName t == name = Just t
      | otherwise = go tps
    go (_:tps) = go tps
    go [] = Nothing

-- | 'remoteFieldFromName' accepts a GraphQL name and searches for its definition
--   in the 'RemoteSchemaIntrospection'.
remoteFieldFromName
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> G.Name
  -> Maybe G.Description
  -> G.Name
  -> G.ArgumentsDefinition RemoteSchemaInputValueDefinition
  -> m (FieldParser n (Field NoFragments RemoteSchemaVariable))
remoteFieldFromName sdoc fieldName description fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing      -> throw400 RemoteSchemaError $ "Could not find type with name " <>> fieldTypeName
    Just typeDef -> remoteField sdoc fieldName description argsDefns typeDef

-- | 'inputValueDefinitionParser' accepts a 'G.InputValueDefinition' and will return an
--   'InputFieldsParser' for it. If a non 'Input' GraphQL type is found in the 'type' of
--    the 'InputValueDefinition' then an error will be thrown.
inputValueDefinitionParser
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> G.InputValueDefinition
  -> m (InputFieldsParser n ((Maybe (InputValue Variable)),(Maybe (HashMap G.Name (Value RemoteSchemaVariable)))))
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal _directives) =
  let doNullability
        :: forall a k . 'Input <: k
        => G.Nullability
        -> Parser k n (Maybe a)
        -> Parser k n (Maybe a)
      doNullability (G.Nullability True)  = fmap join . P.nullable
      doNullability (G.Nullability False) = id

      fieldConstructor
        :: forall k. 'Input <: k
        => Parser k n (Maybe (HashMap G.Name (Value RemoteSchemaVariable)))
        -> InputFieldsParser n ((Maybe (InputValue Variable)), Maybe (HashMap G.Name (Value RemoteSchemaVariable)))
      fieldConstructor parser =
        let wrappedParser :: Parser k n (InputValue Variable, Maybe (HashMap G.Name (Value RemoteSchemaVariable)))
            wrappedParser =
              P.Parser
                { P.pType   = P.pType parser
                , P.pParser = \value ->
                    let inputValP = castWith (P.inputParserInput @k) value
                    in P.pParser parser value <&> (inputValP,)
                }
        in case maybeDefaultVal of
          Nothing ->
            if G.isNullable fieldType
            then fieldOptional name desc wrappedParser <&> f
            else Just <$> field name desc wrappedParser <&> f
          Just defaultVal -> Just <$> fieldWithDefault name desc defaultVal wrappedParser <&> f
        where
          f :: Maybe (InputValue Variable, (Maybe (HashMap G.Name (G.Value RemoteSchemaVariable))))
            -> (Maybe (InputValue Variable), Maybe (HashMap G.Name (G.Value RemoteSchemaVariable)))
          f Nothing                      =  (Nothing, Nothing)
          f (Just (inpValue, presetVal)) =  (Just inpValue, (Map.singleton name . G.VObject) <$> presetVal)


      buildField
        :: G.GType
        -> (forall k. 'Input <: k
               => Parser k n (Maybe (HashMap G.Name (Value RemoteSchemaVariable)))
               -> InputFieldsParser n ((Maybe (InputValue Variable)), (Maybe (HashMap G.Name (G.Value RemoteSchemaVariable)))))
        -> m (InputFieldsParser n ((Maybe (InputValue Variable)), (Maybe (HashMap G.Name (G.Value RemoteSchemaVariable)))))
      buildField fieldType' fieldConstructor' = case fieldType' of
       G.TypeNamed nullability typeName ->
         case lookupType schemaDoc typeName of
           Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <>> typeName
           Just typeDef ->
             case typeDef of
               G.TypeDefinitionScalar scalarTypeDefn ->
                 pure $ fieldConstructor' $ doNullability nullability $ (remoteFieldScalarParser scalarTypeDefn $> Nothing)
               G.TypeDefinitionEnum defn ->
                 pure $ fieldConstructor' $ doNullability nullability $ remoteFieldEnumParser defn $> Nothing
               G.TypeDefinitionObject _ -> throw400 RemoteSchemaError "expected input type, but got output type" -- couldn't find the equivalent error in Validate/Types.hs, so using a new error message
               G.TypeDefinitionInputObject defn -> do
                 potentialObject <- remoteSchemaInputObject schemaDoc defn
                 pure $ case potentialObject of
                   Left dummyInputFieldsParser -> do
                     -- We couln't create a parser, meaning we can't create a field for this
                     -- object. Instead we must return a "pure" InputFieldsParser that always yields
                     -- the needed result without containing a field definition.
                     --
                     -- !!! WARNING #1 !!!
                     -- Since we have no input field in the schema for this field, we can't make the
                     -- distinction between it being actually present at parsing time or not. We
                     -- therefore choose to behave as if it was always present, and we always
                     -- include the preset values in the result.
                     --
                     -- !!! WARNING #2 !!!
                     -- We are re-using an 'InputFieldsParser' that was created earlier! Won't that
                     -- create new fields in the current context? No, it won't, but only because in
                     -- this case we know that it was created from the preset fields in
                     -- 'argumentsParser', and therefore contains no field definition.
                     let dummyInputValue = Just $ GraphQLValue $ G.VObject mempty
                     dummyInputFieldsParser <&> \presets ->
                       (dummyInputValue, (Map.singleton name . G.VObject) <$> presets)
                   Right actualParser -> do
                     -- We're in the normal case: we do have a parser for the input object, which is
                     -- therefore valid (non-empty).
                     fieldConstructor' $ doNullability nullability actualParser
               G.TypeDefinitionUnion _ -> throw400 RemoteSchemaError "expected input type, but got output type"
               G.TypeDefinitionInterface _ -> throw400 RemoteSchemaError "expected input type, but got output type"
       G.TypeList nullability subType ->
         buildField subType (fieldConstructor' . doNullability nullability . fmap fold . P.list)

  in buildField fieldType fieldConstructor

-- | argumentsParser is used for creating an argument parser for remote fields,
--   This function is called for field arguments and input object fields. This
--   function works in the following way:
--   1. All the non-preset arguments are collected and then each of these arguments will
--      be used to call the `inputValueDefinitionParser` function, because we intend
--      these arguments be exposed in the schema
--   2. The preset arguments are collected and converted into a HashMap with the
--      name of the field as the key and the preset value as the value of the hashmap
--   3. Now, after #1, we have a input parser for the non-preset arguments, we combine
--      the current presets with the presets of the non-preset arguments. This is
--      confusing, because it is confusing!
--
--      For example, consider the following input objects:
--
--      input MessageWhereInpObj {
--        id: IntCompareObj
--        name: StringCompareObj
--      }
--
--      input IntCompareObj {
--        eq : Int @preset(value: 2)
--        gt : Int
--        lt : Int
--      }
--
--     When parsing `MessageWhereInpObj`, we see that any of the fields don't have a
--     preset, so we add both of them to the schema. When parsing the `id`
--     field, we see that it's of the input object type, so now, `IntCompareObj` is parsed
--     and one of its three fields have a preset set. So, we build a preset map for `IntCompareObj`
--     which will be `{eq: 2}`. The input parser for `IntCompareObj` will contain this
--     preset map with it. After `IntCompareObj` is parsed, the `MessageWhereInpObj`
--     will continue parsing the `id` field and then it sees that the `IntCompareObj`
--     has a preset associated with it, so now the preset of `IntCompareObj` will be
--     associated with `id`. A new preset map pertinent to `MessageWhereInpObj` will
--     be created, which will be `{id: {eq: 2}}`. So, whenever an incoming query queries
--     for `MessageWhereInpObj` the preset associated will get added to the final arguments
--     map.
argumentsParser
  :: forall n m
  .  (MonadSchema n m, MonadError QErr m)
  => G.ArgumentsDefinition RemoteSchemaInputValueDefinition
  -> RemoteSchemaIntrospection
  -> m (InputFieldsParser n (Maybe (HashMap G.Name (Value RemoteSchemaVariable))))
argumentsParser args schemaDoc = do
  -- ! DANGER !
  --
  -- This function is mutually recursive with 'inputValueDefinitionParser': if one of the non-preset
  -- arguments is an input object, then recursively we'll end up using 'argumentsParser' to parse
  -- its arguments. Note however that if there is no "nonPresetArgs", meaning that all the arguments
  -- have a preset value, then this function will not call 'inputValueDefinitionParser', and will
  -- simply return without any recursion.
  --
  -- This is labelled as dangerous because another function in this module,
  -- 'remoteSchemaInputObject', EXPLICITLY RELIES ON THIS BEHAVIOUR. Due to limitations of the
  -- GraphQL spec and of parser memoization functions, it cannot memoize the case where all
  -- arguments are preset, and therefore relies on the assumption that 'argumentsParser' is not
  -- recursive in this edge case.
  --
  -- This assumptions is unlikely to ever be broken; but if you ever modify this function, please
  -- nonetheless make sure that it is maintained.
  nonPresetArgsParser <- sequenceA <$> for nonPresetArgs (inputValueDefinitionParser schemaDoc)
  let nonPresetArgsParser' = (fmap . fmap) snd nonPresetArgsParser
  pure $ mkPresets <$> nonPresetArgsParser'
  where
    nonPresetArgs =
      map _rsitdDefinition $
      filter (isNothing . _rsitdPresetArgument) args

    currentPreset :: Maybe (HashMap G.Name (Value RemoteSchemaVariable))
    currentPreset =
      let presetArgs' =
            flip mapMaybe args $ \(RemoteSchemaInputValueDefinition inpValDefn preset) ->
                                   (G._ivdName inpValDefn, ) <$> preset
      in case presetArgs' of
           [] -> Nothing
           _  -> Just $ Map.fromList presetArgs'

    mkPresets
      :: [(Maybe (HashMap G.Name (Value RemoteSchemaVariable)))]
      -> Maybe (HashMap G.Name (Value RemoteSchemaVariable))
    mkPresets previousPresets =
      let nestedPreset =
            case catMaybes previousPresets of
              []               -> Nothing
              previousPresets' -> Just $ Map.unions previousPresets'
      in currentPreset <> nestedPreset

-- | 'remoteField' accepts a 'G.TypeDefinition' and will returns a 'FieldParser' for it.
--   Note that the 'G.TypeDefinition' should be of the GraphQL 'Output' kind, when an
--   GraphQL 'Input' kind is provided, then error will be thrown.
remoteField
  :: forall n m
   . (MonadSchema n m, MonadError QErr m)
  => RemoteSchemaIntrospection
  -> G.Name
  -> Maybe G.Description
  -> G.ArgumentsDefinition RemoteSchemaInputValueDefinition
  -> G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition
  -> m (FieldParser n (Field NoFragments RemoteSchemaVariable))
remoteField sdoc fieldName description argsDefn typeDefn = do
  -- TODO add directives
  argsParser <- argumentsParser argsDefn sdoc
  case typeDefn of
    G.TypeDefinitionObject objTypeDefn -> do
      remoteSchemaObjFields <- remoteSchemaObject sdoc objTypeDefn
      -- converting [Field NoFragments Name] to (SelectionSet NoFragments G.Name)
      let remoteSchemaObjSelSet = fmap G.SelectionField <$> remoteSchemaObjFields
      pure remoteSchemaObjSelSet <&> mkFieldParserWithSelectionSet argsParser
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
    -- | This function is used to merge two GraphQL Values. The function is
    --   called from a `Map.union` function, which means that the arguments
    --   to this function come from the same common key of the two HashMaps
    --   that are being merged. The only time the function is called is
    --   when some of the fields of an Input Object fields have preset set
    --   and the remaining input object fields are queried by the user, then
    --   the preset arguments and the user arguments are merged using this function.
    --   For example:
    --
    --   input UserDetails {
    --     id: Int! @preset(value: 1)
    --     name: String!
    --   }
    --
    --   type Mutation {
    --     createUser(details: UserDetails): User
    --   }
    --
    --   Now, since the `id` field already has a preset, the user will not be able
    --   to provide value for it and can only be able to provide the value for `name`.
    --
    --   mutation {
    --     createUser(details: {name: "foo"}) {
    --       name
    --     }
    --   }
    --
    --  When we construct the remote query, we will have a HashMap of the preset
    --  arguments and the user provided arguments. As mentioned earlier, this function
    --  will be called when two maps share a common key, the common key here being
    --  `details`. The preset argument hash map will be `{details: {id: 1}}`
    --  and the user argument `{details: {name: "foo"}}`. Combining these two will
    --  give `{details: {name: "foo", id: 1}}` and then the remote schema is queried
    --  with the merged arguments.
    mergeValue
      :: Maybe (G.Value RemoteSchemaVariable)
      -> Maybe (G.Value RemoteSchemaVariable)
      -> Maybe (G.Value RemoteSchemaVariable)
    mergeValue userArgVal presetArgVal =
      case (userArgVal, presetArgVal) of
        (Just (G.VList   l), Just (G.VList   r)) -> Just $ G.VList $ l <> r
        (Just (G.VObject l), Just (G.VObject r)) -> G.VObject <$> mergeMaps l r
        _                                        -> Nothing
      where
        mergeMaps  l r = sequenceA $ Map.unionWith mergeValue (Just <$> l) (Just <$> r)

    mergeArgs userArgMap presetArgMap =
      sequenceA $ Map.unionWith mergeValue (Just <$> userArgMap) (Just <$> presetArgMap)

    makeField
      :: Maybe G.Name
      -> G.Name
      -> HashMap G.Name (G.Value Variable)
      -> Maybe (HashMap G.Name (G.Value RemoteSchemaVariable))
      -> SelectionSet NoFragments RemoteSchemaVariable
      -> Maybe (G.Field NoFragments RemoteSchemaVariable)
    makeField alias fldName userProvidedArgs presetArgs selSet = do
      let userProvidedArgs' = fmap QueryVariable <$> userProvidedArgs
      resolvedArgs <-
        case presetArgs of
          Just presetArg' -> mergeArgs userProvidedArgs' presetArg'
          Nothing         -> Just userProvidedArgs'
      Just $ G.Field alias fldName resolvedArgs mempty selSet

    validateField
      :: Maybe (G.Field NoFragments RemoteSchemaVariable)
      -> n (G.Field NoFragments RemoteSchemaVariable)
    validateField (Just fld) = pure fld
    -- ideally, we should be throwing a 500 below
    -- The below case, ideally will never happen, because such a query will
    -- not be a valid one and it will fail at the validation stage
    validateField Nothing    = parseErrorWith Unexpected $ "only objects or lists can be merged"

    mkFieldParserWithoutSelectionSet
      :: InputFieldsParser n (Maybe (HashMap G.Name (G.Value RemoteSchemaVariable)))
      -> Parser 'Both n ()
      -> FieldParser n (Field NoFragments RemoteSchemaVariable)
    mkFieldParserWithoutSelectionSet argsParser outputParser =
      -- 'rawSelection' is used here to get the alias and args data
      -- specified to be able to construct the `Field NoFragments G.Name`
      let fieldParser =
            P.rawSelection fieldName description argsParser outputParser
            <&> (\(alias, userProvidedArgs, presetArgs) ->
                   makeField alias fieldName userProvidedArgs presetArgs [])
      in fieldParser `P.bindField` validateField

    mkFieldParserWithSelectionSet
      :: InputFieldsParser n (Maybe (HashMap G.Name (G.Value RemoteSchemaVariable)))
      -> Parser 'Output n (SelectionSet NoFragments RemoteSchemaVariable)
      -> FieldParser n (Field NoFragments RemoteSchemaVariable)
    mkFieldParserWithSelectionSet argsParser outputParser =
      -- 'rawSubselection' is used here to get the alias and args data
      -- specified to be able to construct the `Field NoFragments G.Name`
      let fieldParser =
            P.rawSubselection fieldName description argsParser outputParser
            <&> (\(alias, userProvidedArgs, presetArgs, selSet) ->
                   makeField alias fieldName userProvidedArgs presetArgs selSet)
      in fieldParser `P.bindField` validateField

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
