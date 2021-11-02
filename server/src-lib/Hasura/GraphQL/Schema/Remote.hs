{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

module Hasura.GraphQL.Schema.Remote
  ( buildRemoteParser,
    remoteField,
    customizeFieldParser,
  )
where

import Control.Lens.Extended
  ( Lens',
    set,
    use,
    (%=),
    (^.),
    _1,
    _2,
    _3,
    _4,
  )
import Control.Monad.State.Lazy qualified as Lazy
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.List.NonEmpty qualified as NE
import Data.Monoid (Any (..))
import Data.Parser.JSONPath
import Data.Text qualified as T
import Data.Text.Extended
import Data.Type.Equality
import Hasura.Base.Error
import Hasura.GraphQL.Parser as P
import Hasura.GraphQL.Parser.Internal.Parser qualified as P
import Hasura.GraphQL.Parser.Internal.TypeChecking qualified as P
import Hasura.Prelude
import Hasura.RQL.Types.Common (stringScalar)
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.SchemaCache (IntrospectionResult (IntrospectionResult, irMutationRoot, irQueryRoot, irSubscriptionRoot))
import Language.GraphQL.Draft.Syntax qualified as G

--------------------------------------------------------------------------------
-- Top level function

-- TODO return ParsedIntrospection ?
buildRemoteParser ::
  forall m n.
  (MonadSchema n m, MonadError QErr m) =>
  IntrospectionResult ->
  RemoteSchemaInfo ->
  -- | parsers for, respectively: queries, mutations, and subscriptions
  m
    ( [P.FieldParser n RemoteField],
      Maybe [P.FieldParser n RemoteField],
      Maybe [P.FieldParser n RemoteField]
    )
buildRemoteParser introspectionResult remoteSchemaInfo = do
  (rawQueryParsers, rawMutationParsers, rawSubscriptionParsers) <- buildRawRemoteParser introspectionResult remoteSchemaInfo
  pure $
    evalMemoState $ do
      queryParsers <- customizeFieldParsers remoteSchemaInfo (irQueryRoot introspectionResult) rawQueryParsers
      mutationParsers <- sequence $ customizeFieldParsers remoteSchemaInfo <$> irMutationRoot introspectionResult <*> rawMutationParsers
      subscriptionParsers <- sequence $ customizeFieldParsers remoteSchemaInfo <$> irSubscriptionRoot introspectionResult <*> rawSubscriptionParsers
      pure (queryParsers, mutationParsers, subscriptionParsers)

buildRawRemoteParser ::
  forall m n.
  (MonadSchema n m, MonadError QErr m) =>
  IntrospectionResult ->
  RemoteSchemaInfo ->
  -- | parsers for, respectively: queries, mutations, and subscriptions
  m
    ( [P.FieldParser n RawRemoteField],
      Maybe [P.FieldParser n RawRemoteField],
      Maybe [P.FieldParser n RawRemoteField]
    )
buildRawRemoteParser (IntrospectionResult sdoc queryRoot mutationRoot subscriptionRoot) info@RemoteSchemaInfo {..} = do
  queryT <- makeParsers queryRoot
  mutationT <- makeNonQueryRootFieldParser mutationRoot $$(G.litName "Mutation")
  subscriptionT <- makeNonQueryRootFieldParser subscriptionRoot $$(G.litName "Subscription")
  return (queryT, mutationT, subscriptionT)
  where
    makeFieldParser :: G.FieldDefinition RemoteSchemaInputValueDefinition -> m (P.FieldParser n RawRemoteField)
    makeFieldParser fieldDef = do
      fldParser <- remoteFieldFromDefinition sdoc fieldDef
      pure $ RemoteFieldG info mempty <$> fldParser

    makeParsers :: G.Name -> m [P.FieldParser n RawRemoteField]
    makeParsers rootName =
      case lookupType sdoc rootName of
        Just (G.TypeDefinitionObject o) ->
          traverse makeFieldParser $ G._otdFieldsDefinition o
        _ -> throw400 Unexpected $ rootName <<> " has to be an object type"

    makeNonQueryRootFieldParser :: Maybe G.Name -> G.Name -> m (Maybe [P.FieldParser n RawRemoteField])
    makeNonQueryRootFieldParser userProvidedRootName defaultRootName =
      case userProvidedRootName of
        Just _rootName -> traverse makeParsers userProvidedRootName
        Nothing ->
          let isDefaultRootObjectExists = isJust $ lookupObject sdoc defaultRootName
           in bool (pure Nothing) (traverse makeParsers $ Just defaultRootName) $ isDefaultRootObjectExists

--------------------------------------------------------------------------------
-- Remote schema input parsers

{- Note [Variable expansion in remote schema input parsers]

### Input parsers as lightweight type checkers

The purpose of input parsers for remote schemas is not to translate the provided input values into
an internal representation: those values will be transmitted more or less unmodified to the remote
service; their main purpose is simply to check the shape of the input against the remote schema.

Consider, for instance, the following remote schema:

    input Foo {
      bar: Int!
    }

    type Query {
      run(foo: Foo!): Int!
    }

Our parsers will need to decide which invocations of `run` are valid:

    query {
      run(null)             # invalid: foo is non-nullable
      run(foo: {baz: 0})    # invalid: Foo doesn't have a "baz" field
      run(foo: {bar: "0"})  # actually valid!
    }

That last example is surprising: why would we accept a string literal for an Int? It simply is
because we delegate the task of translating the literal into a scalar to the remote server. After
all, *we* advertise some values as Int in the schema, despite accepting string literals.

### Inserting remote permissions presets

Where things get more complicated is with remote permissions. We allow users to specify "presets":
values that will always be provided to the remote schema, and that the user cannot customize in
their query. For instance, given the following schema with permissions:

    input Range {
      low:  Int! @preset(value: 0)
      high: Int!
    }

    type Query {
      getValues(range: Range!): [Int]
    }

a user cannot specify "low" in OUR schema, as we will insert its value when parsing the incoming
query. This is the second purpose of those input parsers: they insert remote schema presets where
required. In this case:

    # we receive
    query {
      getValues(range: {high: 42})
    }

    # we emit
    query {
      getValues(range: {low: 0, high: 42})
    }

### Variable expansion

But where this gets even more complicated is with variables. As much as possible, we simply forward
variables without interpeting them (not all JSON values are representable in GraphQL). We do so
whenever possible; for instance, using the previously established remote schema:

    # we receive
    query:
      query($h: Int!) {
        getValues(range: {high: $h})
      }
    variables:
      { "h": 42 }

    # we emit
    query:
      query($h: Int!) {
        getValues(range: {low: 0, high: $h})
      }
    variables:
      { "h": 42 }

The tricky case is when a preset field is *within a variable*. We then have no choice: we have to
expand the variable, and rewrap the value as best as we can, to minimize the amount of JSON
evaluation. For instance:

    # we receive
    query:
      query($r: Range!) {
        getValues(range: $r)
      }
    variables:
      { "r": {"high": 42} }

    # we emit
    query:
      query($hasura_json_var_1: Int!) {
        getValues(range: {low: 0, high: $hasura_json_var_1})
      }
    variables:
      { "hasura_json_var_1": 42 }

Our parsers, like all others in our model, expand the variables as they traverse the tree, and add
the preset values where required. But the downside of this is that we will create one such JSON
variable per scalar within a JSON variable!

### Short-circuiting optimization

To avoid this, we track in the parsers whether an alteration has occured: if we had to insert a
preset value. As long as we don't, we can discard the output of the parser, as it will contain the
exact same value as the input (if perhaps represented differently); by discarding the output and
just forwarding the input, we avoid expanding variables if no preset needs be inserted.
-}

-- | Helper, used to track whether an input value was altered during its parsing. At time of
-- writing, the only possible source of alteration is preset values. They might force evaluation of
-- variables, and encapsulation of sub-JSON expressions as new variables. Each parser indicates
-- whether such alteration took place within its part of the tree.
-- See Note [Variable expansion in remote schema input parsers] for more information.
newtype Altered = Altered {getAltered :: Bool}
  deriving (Show)
  deriving (Semigroup, Monoid) via Any

-- | 'inputValueDefinitionParser' accepts a 'G.InputValueDefinition' and will return an
-- 'InputFieldsParser' for it. If a non 'Input' GraphQL type is found in the 'type' of the
-- 'InputValueDefinition' then an error will be thrown.
--
-- Each parser also returns a boolean that indicates whether the parsed value was altered by
-- presets. Presets might force the evaluation of variables that would otherwise be transmitted
-- unmodified.
inputValueDefinitionParser ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.InputValueDefinition ->
  m (InputFieldsParser n (Maybe (Altered, G.Value RemoteSchemaVariable)))
inputValueDefinitionParser schemaDoc (G.InputValueDefinition desc name fieldType maybeDefaultVal _directives) =
  buildField fieldConstructor fieldType
  where
    doNullability ::
      forall a k.
      'Input <: k =>
      G.Nullability ->
      Parser k n (Maybe a) ->
      Parser k n (Maybe a)
    doNullability (G.Nullability True) = fmap join . P.nullable
    doNullability (G.Nullability False) = id

    fieldConstructor ::
      forall k.
      'Input <: k =>
      Parser k n (Maybe (Altered, G.Value RemoteSchemaVariable)) ->
      InputFieldsParser n (Maybe (Altered, G.Value RemoteSchemaVariable))
    fieldConstructor (shortCircuitIfUnaltered -> parser) =
      case maybeDefaultVal of
        Nothing ->
          if G.isNullable fieldType
            then join <$> fieldOptional name desc parser
            else field name desc parser
        Just defaultVal -> fieldWithDefault name desc defaultVal parser

    buildField ::
      ( forall k.
        'Input <: k =>
        Parser k n (Maybe (Altered, G.Value RemoteSchemaVariable)) ->
        InputFieldsParser n (Maybe (Altered, G.Value RemoteSchemaVariable))
      ) ->
      G.GType ->
      m (InputFieldsParser n (Maybe (Altered, G.Value RemoteSchemaVariable)))
    buildField mkInputFieldsParser = \case
      G.TypeNamed nullability typeName ->
        case lookupType schemaDoc typeName of
          Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <>> typeName
          Just typeDef ->
            case typeDef of
              G.TypeDefinitionScalar scalarTypeDefn ->
                pure $ mkInputFieldsParser $ doNullability nullability $ Just <$> remoteFieldScalarParser scalarTypeDefn
              G.TypeDefinitionEnum defn ->
                pure $ mkInputFieldsParser $ doNullability nullability $ Just <$> remoteFieldEnumParser defn
              G.TypeDefinitionObject _ ->
                throw400 RemoteSchemaError "expected input type, but got output type"
              G.TypeDefinitionInputObject defn -> do
                potentialObject <- remoteInputObjectParser schemaDoc defn
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
                    Just <$> dummyInputFieldsParser
                  Right actualParser -> do
                    -- We're in the normal case: we do have a parser for the input object, which is
                    -- therefore valid (non-empty).
                    mkInputFieldsParser $ doNullability nullability $ Just <$> actualParser
              G.TypeDefinitionUnion _ ->
                throw400 RemoteSchemaError "expected input type, but got output type"
              G.TypeDefinitionInterface _ ->
                throw400 RemoteSchemaError "expected input type, but got output type"
      G.TypeList nullability subType -> do
        buildField (mkInputFieldsParser . doNullability nullability . fmap (Just . fmap G.VList . aggregateListAndAlteration) . P.list) subType

-- | remoteFieldScalarParser attempts to parse a scalar value for a given remote field
--
-- We do not attempt to verify that the literal is correct! Some GraphQL implementations, including
-- ours, are a bit flexible with the intepretations of literals; for instance, there are several
-- places in our schema where we declare something to be an `Int`, but actually accept `String`
-- literals. We do however peform variable type-checking.
--
-- If we encounter a JSON value, it means that we were introspecting a query variable. To call the
-- remote schema, we need a graphql value; we therefore need to treat that JSON expression as if it
-- were a query variable of its own. To avoid ending up with one such variable per scalar in the
-- query, we also track alterations, to apply optimizations.
-- See Note [Variable expansion in remote schema input parsers] for more information.
remoteFieldScalarParser ::
  MonadParse n =>
  G.ScalarTypeDefinition ->
  P.Parser 'Both n (Altered, G.Value RemoteSchemaVariable)
remoteFieldScalarParser (G.ScalarTypeDefinition description name _directives) =
  P.Parser
    { pType = schemaType,
      pParser = \inputValue ->
        (Altered False,) <$> case inputValue of
          JSONValue v -> pure $ G.VVariable $ RemoteJSONValue gType v
          GraphQLValue v -> for v \var -> do
            P.typeCheck False gType var
            pure $ QueryVariable var
    }
  where
    schemaType = NonNullable $ TNamed $ mkDefinition (Typename name) description TIScalar
    gType = toGraphQLType schemaType

remoteFieldEnumParser ::
  MonadParse n =>
  G.EnumTypeDefinition ->
  Parser 'Both n (Altered, G.Value RemoteSchemaVariable)
remoteFieldEnumParser (G.EnumTypeDefinition desc name _directives valueDefns) =
  let enumValDefns =
        valueDefns <&> \(G.EnumValueDefinition enumDesc enumName _) ->
          ( mkDefinition (G.unEnumValue enumName) enumDesc P.EnumValueInfo,
            G.VEnum enumName
          )
   in fmap (Altered False,) $ P.enum (Typename name) desc $ NE.fromList enumValDefns

-- | remoteInputObjectParser returns an input parser for a given 'G.InputObjectTypeDefinition'
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
remoteInputObjectParser ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition ->
  m
    ( Either
        (InputFieldsParser n (Altered, G.Value RemoteSchemaVariable))
        (Parser 'Input n (Altered, G.Value RemoteSchemaVariable))
    )
remoteInputObjectParser schemaDoc defn@(G.InputObjectTypeDefinition desc name _ valueDefns) =
  if all (isJust . _rsitdPresetArgument) valueDefns
    then -- All the fields are preset: we can't create a parser, that would result in an invalid type in
    -- the schema (an input object with no field). We therefore forward the InputFieldsParser
    -- unmodified. No need to memoize this branch: since all arguments are preset, 'argumentsParser'
    -- won't be recursively calling this function.
      Left . fmap (fmap G.VObject) <$> argumentsParser valueDefns schemaDoc
    else -- At least one field is not a preset, meaning we have the guarantee that there will be at least
    -- one field in the input object. We have to memoize this branch as we might recursively call
    -- the same parser.

      Right <$> P.memoizeOn 'remoteInputObjectParser defn do
        argsParser <- argumentsParser valueDefns schemaDoc
        pure $ fmap G.VObject <$> P.object (Typename name) desc argsParser

-- | Variable expansion optimization.
-- Since each parser returns a value that indicates whether it was altered, we can detect when no
-- alteration took place, and replace the parsed and expanded value by its original.
-- See Note [Variable expansion in remote schema input parsers] for more information.
shortCircuitIfUnaltered ::
  forall k n.
  ('Input <: k, MonadParse n) =>
  Parser k n (Maybe (Altered, G.Value RemoteSchemaVariable)) ->
  Parser k n (Maybe (Altered, G.Value RemoteSchemaVariable))
shortCircuitIfUnaltered parser =
  P.Parser
    { pType = P.pType parser,
      pParser = \value -> do
        result <- P.pParser parser value
        pure $ case result of
          -- The parser did yield a value, and it was unmodified by presets
          -- we can short-citcuit by transforming the input value, therefore
          -- "unpeeling" variables and avoiding extraneous JSON variables.
          Just (Altered False, _) -> Just $
            (Altered False,) $ case castWith (P.inputParserInput @k) value of
              -- The input was a GraphQL value: just forward it.
              GraphQLValue v -> QueryVariable <$> v
              -- The input value was already a JSON value: we still have to create
              -- a new JSON variable, but it will still be more efficient than having
              -- all the leaves of said value each be their own distinct value.
              JSONValue v -> G.VVariable $ RemoteJSONValue (toGraphQLType $ P.pType parser) v
          -- Otherwise either the parser did not yield any value, or a value
          -- that has been altered by presets and permissions; we forward it
          -- unoptimized.
          _ -> result
    }

-- | argumentsParser is used for creating an argument parser for remote fields,
-- This function is called for field arguments and input object fields. This
-- function works in the following way:
--
--   * if a field is not preset, we recursively call `inputValueDefinitionParser` on it
--   * otherwise, we use the preset
--
-- For example, consider the following input objects:
--
--   input MessageWhereInpObj {
--     id:   IntCompareObj
--     name: StringCompareObj
--   }
--
--   input IntCompareObj {
--     eq : Int @preset(value: 2)
--     gt : Int
--     lt : Int
--   }
--
-- parsing a MessageWhereInpObj will result in the following call tree:
--
--   -> argumentsParser MessageWhereInpObj
--     -> id => inputValueDefinitionParser IntCompareObj
--       -> remoteInputObjectParser IntCompareObj
--         -> argumentsParser IntCompareObj
--           -> eq => using preset, no recursion
--           -> gt => inputValueDefinitionParser Int
--             -> remoteFieldScalarParser Int
--           -> lt => inputValueDefinitionParser Int
--             -> remoteFieldScalarParser Int
--     -> name => inputValueDefinitionParser StringCompareObj
--       -> ...
--
-- Furthermore, like all other input parsers in this file, 'argumentsParser' indicates whether this
-- part of the tree was altered during parsing; if any of the fields is preset, or recursively
-- contains values that contain presets further down, then this result is labelled as altered.
argumentsParser ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  G.ArgumentsDefinition RemoteSchemaInputValueDefinition ->
  RemoteSchemaIntrospection ->
  m (InputFieldsParser n (Altered, HashMap G.Name (G.Value RemoteSchemaVariable)))
argumentsParser args schemaDoc = do
  -- ! DANGER !
  --
  -- This function is mutually recursive with 'inputValueDefinitionParser': if one of the non-preset
  -- arguments is an input object, then recursively we'll end up using 'argumentsParser' to parse
  -- its arguments. Note however that if all arguments have a preset value, then this function will
  -- not call 'inputValueDefinitionParser', and will simply return without any recursion.
  --
  -- This is labelled as dangerous because another function in this module,
  -- 'remoteInputObjectParser', EXPLICITLY RELIES ON THIS BEHAVIOUR. Due to limitations of the
  -- GraphQL spec and of parser memoization functions, it cannot memoize the case where all
  -- arguments are preset, and therefore relies on the assumption that 'argumentsParser' is not
  -- recursive in this edge case.
  --
  -- This assumptions is unlikely to ever be broken; but if you ever modify this function, please
  -- nonetheless make sure that it is maintained.
  argsParsers <- for args \arg -> do
    let argDef = _rsitdDefinition arg
        argName = G._ivdName argDef
    argParser <- case _rsitdPresetArgument arg of
      Nothing -> inputValueDefinitionParser schemaDoc argDef
      -- This is the source of all possible alterations: one of the fields is preset; everything
      -- "above" this field in the tree will be considered "altered", and the optimizations will
      -- not apply.
      Just preset -> pure $ pure $ pure (Altered True, preset)
    pure $ fmap (fmap (argName,)) <$> argParser
  pure $ sequenceA argsParsers <&> fmap Map.fromList . aggregateListAndAlteration

aggregateListAndAlteration :: [Maybe (Altered, a)] -> (Altered, [a])
aggregateListAndAlteration = first mconcat . unzip . catMaybes

--------------------------------------------------------------------------------
-- Remote schema output parsers

-- | 'remoteSchemaObject' returns a output parser for a given 'ObjectTypeDefinition'.
remoteSchemaObject ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.ObjectTypeDefinition RemoteSchemaInputValueDefinition ->
  m (Parser 'Output n [G.Field G.NoFragments RemoteSchemaVariable])
remoteSchemaObject schemaDoc defn@(G.ObjectTypeDefinition description name interfaces _directives subFields) =
  P.memoizeOn 'remoteSchemaObject defn do
    subFieldParsers <- traverse (remoteFieldFromDefinition schemaDoc) subFields
    interfaceDefs <- traverse getInterface interfaces
    implements <- traverse (remoteSchemaInterface schemaDoc) interfaceDefs
    -- TODO: also check sub-interfaces, when these are supported in a future graphql spec
    traverse_ validateImplementsFields interfaceDefs
    pure $
      P.selectionSetObject (Typename name) description subFieldParsers implements
        <&> toList
          . OMap.mapWithKey
            ( \alias -> handleTypename $ \_ ->
                G.Field (Just alias) $$(G.litName "__typename") mempty mempty mempty
            )
  where
    getInterface :: G.Name -> m (G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition)
    getInterface interfaceName =
      onNothing (lookupInterface schemaDoc interfaceName) $
        throw400 RemoteSchemaError $
          "Could not find interface " <> squote interfaceName
            <> " implemented by Object type "
            <> squote name
    validateImplementsFields :: G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition -> m ()
    validateImplementsFields interface =
      traverse_ (validateImplementsField (G._itdName interface)) (G._itdFieldsDefinition interface)
    validateImplementsField :: G.Name -> G.FieldDefinition RemoteSchemaInputValueDefinition -> m ()
    validateImplementsField interfaceName interfaceField =
      case lookup (G._fldName interfaceField) (zip (fmap G._fldName subFields) subFields) of
        Nothing ->
          throw400 RemoteSchemaError $
            "Interface field " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
              <> " expected, but "
              <> squote name
              <> " does not provide it"
        Just f -> do
          unless (validateSubType (G._fldType f) (G._fldType interfaceField)) $
            throw400 RemoteSchemaError $
              "The type of Object field " <> squote name <> "." <> dquote (G._fldName f)
                <> " ("
                <> G.showGT (G._fldType f)
                <> ") is not the same type/sub type of Interface field "
                <> squote interfaceName
                <> "."
                <> dquote (G._fldName interfaceField)
                <> " ("
                <> G.showGT (G._fldType interfaceField)
                <> ")"
          traverse_
            ( validateArgument
                (map _rsitdDefinition (G._fldArgumentsDefinition f))
                . _rsitdDefinition
            )
            (G._fldArgumentsDefinition interfaceField)
          traverse_
            ( validateNoExtraNonNull
                (map _rsitdDefinition (G._fldArgumentsDefinition interfaceField))
                . _rsitdDefinition
            )
            (G._fldArgumentsDefinition f)
          where
            validateArgument :: [G.InputValueDefinition] -> G.InputValueDefinition -> m ()
            validateArgument objectFieldArgs ifaceArgument =
              case lookup (G._ivdName ifaceArgument) (zip (fmap G._ivdName objectFieldArgs) objectFieldArgs) of
                Nothing ->
                  throw400 RemoteSchemaError $
                    "Interface field argument " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
                      <> "("
                      <> dquote (G._ivdName ifaceArgument)
                      <> ":) required, but Object field "
                      <> squote name
                      <> "."
                      <> dquote (G._fldName f)
                      <> " does not provide it"
                Just a ->
                  unless (G._ivdType a == G._ivdType ifaceArgument) $
                    throw400 RemoteSchemaError $
                      "Interface field argument " <> squote interfaceName <> "." <> dquote (G._fldName interfaceField)
                        <> "("
                        <> dquote (G._ivdName ifaceArgument)
                        <> ":) expects type "
                        <> G.showGT (G._ivdType ifaceArgument)
                        <> ", but "
                        <> squote name
                        <> "."
                        <> dquote (G._fldName f)
                        <> "("
                        <> dquote (G._ivdName ifaceArgument)
                        <> ":) has type "
                        <> G.showGT (G._ivdType a)
            validateNoExtraNonNull :: [G.InputValueDefinition] -> G.InputValueDefinition -> m ()
            validateNoExtraNonNull ifaceArguments objectFieldArg =
              case lookup (G._ivdName objectFieldArg) (zip (fmap G._ivdName ifaceArguments) ifaceArguments) of
                Just _ -> pure ()
                Nothing ->
                  unless (G.isNullable (G._ivdType objectFieldArg)) $
                    throw400 RemoteSchemaError $
                      "Object field argument " <> squote name <> "." <> dquote (G._fldName f) <> "("
                        <> dquote (G._ivdName objectFieldArg)
                        <> ":) is of required type "
                        <> G.showGT (G._ivdType objectFieldArg)
                        <> ", but is not provided by Interface field "
                        <> squote interfaceName
                        <> "."
                        <> dquote (G._fldName interfaceField)
    validateSubType :: G.GType -> G.GType -> Bool
    -- TODO this ignores nullability which is probably wrong, even though the GraphQL spec is ambiguous
    validateSubType (G.TypeList _ x) (G.TypeList _ y) = validateSubType x y
    -- It is OK to "upgrade" the strictness
    validateSubType (G.TypeNamed (G.Nullability False) x) (G.TypeNamed (G.Nullability True) y) =
      validateSubType (G.TypeNamed (G.Nullability True) x) (G.TypeNamed (G.Nullability True) y)
    validateSubType (G.TypeNamed nx x) (G.TypeNamed ny y) =
      case (lookupType schemaDoc x, lookupType schemaDoc y) of
        (Just x', Just y') -> nx == ny && validateSubTypeDefinition x' y'
        _ -> False
    validateSubType _ _ = False
    validateSubTypeDefinition x' y' | x' == y' = True
    validateSubTypeDefinition (G.TypeDefinitionObject otd) (G.TypeDefinitionInterface itd) =
      G._otdName otd `elem` G._itdPossibleTypes itd
    validateSubTypeDefinition (G.TypeDefinitionObject _otd) (G.TypeDefinitionUnion _utd) =
      True -- TODO write appropriate check (may require saving 'possibleTypes' in Syntax.hs)
    validateSubTypeDefinition _ _ = False

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
remoteSchemaInterface ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition ->
  m (Parser 'Output n (G.SelectionSet G.NoFragments RemoteSchemaVariable))
remoteSchemaInterface schemaDoc defn@(G.InterfaceTypeDefinition description name _directives fields possibleTypes) =
  P.memoizeOn 'remoteSchemaObject defn do
    subFieldParsers <- traverse (remoteFieldFromDefinition schemaDoc) fields
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
    pure $ P.selectionSetInterface (Typename name) description subFieldParsers objs <&> constructInterfaceSelectionSet
  where
    getObject :: G.Name -> m (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing ->
            throw400 RemoteSchemaError $
              "Could not find type " <> squote objectName
                <> ", which is defined as a member type of Interface "
                <> squote name
          Just _ ->
            throw400 RemoteSchemaError $
              "Interface type " <> squote name
                <> " can only include object types. It cannot include "
                <> squote objectName

    -- 'constructInterfaceQuery' constructs a remote interface query.
    constructInterfaceSelectionSet ::
      [(G.Name, [G.Field G.NoFragments RemoteSchemaVariable])] ->
      G.SelectionSet G.NoFragments RemoteSchemaVariable
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

          allTheSame (x : xs) | all (== x) xs = Just x
          allTheSame _ = Nothing

          -- #2 of Note [Querying remote schema interface fields]
          nonCommonInterfaceFields =
            catMaybes $
              flip map objNameAndFields $ \(objName, objFields) ->
                let nonCommonFields = filter (not . flip elem commonInterfaceFields) objFields
                 in mkObjInlineFragment (objName, map G.SelectionField nonCommonFields)

          -- helper function for #4 of Note [Querying remote schema interface fields]
          mkObjInlineFragment (_, []) = Nothing
          mkObjInlineFragment (objName, selSet) =
            Just $
              G.SelectionInlineFragment $
                G.InlineFragment (Just objName) mempty selSet
       in -- #5 of Note [Querying remote schema interface fields]
          fmap G.SelectionField commonInterfaceFields <> nonCommonInterfaceFields

-- | 'remoteSchemaUnion' returns a output parser for a given 'UnionTypeDefinition'.
remoteSchemaUnion ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.UnionTypeDefinition ->
  m (Parser 'Output n (G.SelectionSet G.NoFragments RemoteSchemaVariable))
remoteSchemaUnion schemaDoc defn@(G.UnionTypeDefinition description name _directives objectNames) =
  P.memoizeOn 'remoteSchemaObject defn do
    objs <- traverse (getObjectParser schemaDoc getObject) objectNames
    when (null objs) $
      throw400 RemoteSchemaError $ "List of member types cannot be empty for union type " <> squote name
    pure $
      P.selectionSetUnion (Typename name) description objs
        <&> ( \objNameAndFields ->
                catMaybes $
                  objNameAndFields <&> \(objName, fields) ->
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
                      _ ->
                        Just
                          ( G.SelectionInlineFragment $
                              G.InlineFragment (Just objName) mempty $ fmap G.SelectionField fields
                          )
            )
  where
    getObject :: G.Name -> m (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
    getObject objectName =
      onNothing (lookupObject schemaDoc objectName) $
        case lookupInterface schemaDoc objectName of
          Nothing ->
            throw400 RemoteSchemaError $
              "Could not find type " <> squote objectName
                <> ", which is defined as a member type of Union "
                <> squote name
          Just _ ->
            throw400 RemoteSchemaError $
              "Union type " <> squote name
                <> " can only include object types. It cannot include "
                <> squote objectName

remoteFieldFromDefinition ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.FieldDefinition RemoteSchemaInputValueDefinition ->
  m (FieldParser n (G.Field G.NoFragments RemoteSchemaVariable))
remoteFieldFromDefinition schemaDoc (G.FieldDefinition description name argsDefinition gType _) =
  let addNullableList :: FieldParser n (G.Field G.NoFragments RemoteSchemaVariable) -> FieldParser n (G.Field G.NoFragments RemoteSchemaVariable)
      addNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser) =
        P.FieldParser (Definition name' un desc (FieldInfo args (Nullable (TList typ)))) parser

      addNonNullableList :: FieldParser n (G.Field G.NoFragments RemoteSchemaVariable) -> FieldParser n (G.Field G.NoFragments RemoteSchemaVariable)
      addNonNullableList (P.FieldParser (Definition name' un desc (FieldInfo args typ)) parser) =
        P.FieldParser (Definition name' un desc (FieldInfo args (NonNullable (TList typ)))) parser

      -- TODO add directives, deprecation
      convertType :: G.GType -> m (FieldParser n (G.Field G.NoFragments RemoteSchemaVariable))
      convertType gType' = do
        case gType' of
          G.TypeNamed (G.Nullability True) fieldTypeName ->
            P.nullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (G.Nullability True) gType'' ->
            addNullableList <$> convertType gType''
          G.TypeNamed (G.Nullability False) fieldTypeName -> do
            P.nonNullableField <$> remoteFieldFromName schemaDoc name description fieldTypeName argsDefinition
          G.TypeList (G.Nullability False) gType'' ->
            addNonNullableList <$> convertType gType''
   in convertType gType

-- | 'remoteFieldFromName' accepts a GraphQL name and searches for its definition
--   in the 'RemoteSchemaIntrospection'.
remoteFieldFromName ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe G.Description ->
  G.Name ->
  G.ArgumentsDefinition RemoteSchemaInputValueDefinition ->
  m (FieldParser n (G.Field G.NoFragments RemoteSchemaVariable))
remoteFieldFromName sdoc fieldName description fieldTypeName argsDefns =
  case lookupType sdoc fieldTypeName of
    Nothing -> throw400 RemoteSchemaError $ "Could not find type with name " <>> fieldTypeName
    Just typeDef -> remoteField sdoc fieldName description argsDefns typeDef

-- | 'remoteField' accepts a 'G.TypeDefinition' and will returns a 'FieldParser' for it.
--   Note that the 'G.TypeDefinition' should be of the GraphQL 'Output' kind, when an
--   GraphQL 'Input' kind is provided, then error will be thrown.
remoteField ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  G.Name ->
  Maybe G.Description ->
  G.ArgumentsDefinition RemoteSchemaInputValueDefinition ->
  G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition ->
  m (FieldParser n (G.Field G.NoFragments RemoteSchemaVariable))
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
      pure $ mkFieldParserWithoutSelectionSet argsParser $ void $ remoteFieldScalarParser scalarTypeDefn
    G.TypeDefinitionEnum enumTypeDefn ->
      pure $ mkFieldParserWithoutSelectionSet argsParser $ void $ remoteFieldEnumParser enumTypeDefn
    G.TypeDefinitionInterface ifaceTypeDefn ->
      remoteSchemaInterface sdoc ifaceTypeDefn <&> mkFieldParserWithSelectionSet argsParser
    G.TypeDefinitionUnion unionTypeDefn ->
      remoteSchemaUnion sdoc unionTypeDefn <&> mkFieldParserWithSelectionSet argsParser
    _ -> throw400 RemoteSchemaError "expected output type, but got input type"
  where
    mkField ::
      Maybe G.Name ->
      HashMap G.Name (G.Value RemoteSchemaVariable) ->
      G.SelectionSet G.NoFragments RemoteSchemaVariable ->
      G.Field G.NoFragments RemoteSchemaVariable
    mkField alias args selSet =
      G.Field alias fieldName args mempty selSet

    mkFieldParserWithoutSelectionSet ::
      InputFieldsParser n (Altered, HashMap G.Name (G.Value RemoteSchemaVariable)) ->
      Parser 'Both n () ->
      FieldParser n (G.Field G.NoFragments RemoteSchemaVariable)
    mkFieldParserWithoutSelectionSet argsParser outputParser =
      P.rawSelection fieldName description argsParser outputParser
        <&> \(alias, _, (_, args)) -> mkField alias args []

    mkFieldParserWithSelectionSet ::
      InputFieldsParser n (Altered, HashMap G.Name (G.Value RemoteSchemaVariable)) ->
      Parser 'Output n (G.SelectionSet G.NoFragments RemoteSchemaVariable) ->
      FieldParser n (G.Field G.NoFragments RemoteSchemaVariable)
    mkFieldParserWithSelectionSet argsParser outputParser =
      P.rawSubselection fieldName description argsParser outputParser
        <&> \(alias, _, (_, args), selSet) -> mkField alias args selSet

-- | helper function to get a parser of an object with it's name
--   This function is called from 'remoteSchemaInterface' and
--   'remoteSchemaObject' functions. Both of these have a slightly
--   different implementation of 'getObject', which is the
--   reason 'getObject' is an argument to this function
getObjectParser ::
  forall n m.
  (MonadSchema n m, MonadError QErr m) =>
  RemoteSchemaIntrospection ->
  (G.Name -> m (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)) ->
  G.Name ->
  m (Parser 'Output n (G.Name, [G.Field G.NoFragments RemoteSchemaVariable]))
getObjectParser schemaDoc getObject objName = do
  obj <- remoteSchemaObject schemaDoc =<< getObject objName
  return $ (objName,) <$> obj

addCustomNamespace ::
  forall m.
  MonadParse m =>
  RemoteSchemaInfo ->
  G.Name ->
  G.Name ->
  [P.FieldParser m RawRemoteField] ->
  P.FieldParser m RemoteField
addCustomNamespace remoteSchemaInfo rootTypeName namespace fieldParsers =
  P.subselection_ namespace Nothing remoteFieldParser
  where
    rawRemoteFieldsParser :: Parser 'Output m [RawRemoteField]
    rawRemoteFieldsParser =
      P.selectionSet (Typename rootTypeName) Nothing fieldParsers
        <&> toList
          . OMap.mapWithKey
            ( \alias -> \case
                P.SelectField fld -> fld
                P.SelectTypename fld ->
                  -- In P.selectionSet we lose the resultCustomizer from __typename fields so we need to put it back
                  let resultCustomizer = modifyFieldByName alias $ customizeTypeNameString $ _rscCustomizeTypeName $ rsCustomizer remoteSchemaInfo
                   in RemoteFieldG remoteSchemaInfo resultCustomizer $ G.Field (Just alias) $$(G.litName "__typename") mempty mempty mempty
            )

    remoteFieldParser :: Parser 'Output m RemoteField
    remoteFieldParser =
      rawRemoteFieldsParser <&> \remoteFields ->
        RemoteFieldG
          remoteSchemaInfo
          (foldMap _rfResultCustomizer remoteFields)
          (RRFNamespaceField $ G.SelectionField . _rfField <$> remoteFields)

customizeFieldParsers ::
  forall m n.
  (MonadState MemoState m, MonadFix m, MonadParse n) =>
  RemoteSchemaInfo ->
  G.Name ->
  [P.FieldParser n RawRemoteField] ->
  m [P.FieldParser n RemoteField]
customizeFieldParsers remoteSchemaInfo@RemoteSchemaInfo {..} rootTypeName fieldParsers = do
  fieldParsers' <-
    if hasTypeOrFieldCustomizations rsCustomizer
      then traverse (customizeFieldParser' (set rfResultCustomizer) rsCustomizer rootTypeName) fieldParsers
      else -- no need to customize individual FieldParsers if there are no type or field name customizations
        pure fieldParsers
  pure $ case _rscNamespaceFieldName rsCustomizer of
    Nothing -> fmap realRemoteField <$> fieldParsers'
    Just namespace -> [addCustomNamespace remoteSchemaInfo rootTypeName namespace fieldParsers']

customizeFieldParser ::
  forall n a b.
  (MonadParse n) =>
  (ResultCustomizer -> a -> b) ->
  RemoteSchemaCustomizer ->
  G.Name ->
  P.FieldParser n a ->
  (P.FieldParser n b)
customizeFieldParser setResultCustomizer remoteSchemaCustomizer rootTypeName =
  if hasTypeOrFieldCustomizations remoteSchemaCustomizer
    then evalMemoState . customizeFieldParser' setResultCustomizer remoteSchemaCustomizer rootTypeName
    else fmap $ setResultCustomizer mempty

customizeFieldParser' ::
  forall m n a b.
  (MonadState MemoState m, MonadFix m, MonadParse n) =>
  (ResultCustomizer -> a -> b) ->
  RemoteSchemaCustomizer ->
  G.Name ->
  P.FieldParser n a ->
  m (P.FieldParser n b)
customizeFieldParser' setResultCustomizer remoteSchemaCustomizer rootTypeName P.FieldParser {..} = do
  customizedDefinition <- customizeFieldDefinition remoteSchemaCustomizer rootTypeName fDefinition
  let customizedRootTypeName = remoteSchemaCustomizeTypeName remoteSchemaCustomizer rootTypeName
  pure
    P.FieldParser
      { fParser =
          fParserWithResultCustomizer
            <=< customizeField customizedRootTypeName (dInfo customizedDefinition) . fmap customizeVariable,
        fDefinition = customizedDefinition
      }
  where
    fParserWithResultCustomizer :: (ResultCustomizer, G.Field G.NoFragments Variable) -> n b
    fParserWithResultCustomizer (resultCustomizer, fld) =
      setResultCustomizer resultCustomizer <$> fParser fld

    customizeVariable :: Variable -> Variable
    customizeVariable Variable {..} = Variable {vType = customizeGraphQLType vType, ..}

    customizeGraphQLType :: G.GType -> G.GType
    customizeGraphQLType = \case
      G.TypeNamed nullability name -> G.TypeNamed nullability $ remoteSchemaDecustomizeTypeName remoteSchemaCustomizer name
      G.TypeList nullability gtype -> G.TypeList nullability $ customizeGraphQLType gtype

    customizeField :: G.Name -> P.FieldInfo -> G.Field G.NoFragments var -> n (ResultCustomizer, G.Field G.NoFragments var)
    customizeField parentTypeName (P.FieldInfo _ fieldType) (G.Field alias fieldName args directives selSet) = do
      let fieldName' =
            if "__" `T.isPrefixOf` G.unName fieldName
              then fieldName
              else remoteSchemaDecustomizeFieldName remoteSchemaCustomizer parentTypeName fieldName
          alias' = alias <|> if fieldName' == fieldName then Nothing else Just fieldName
      selSet' :: [(ResultCustomizer, G.Selection G.NoFragments var)] <- withPath (++ [Key "selectionSet"]) $
        case fieldType ^. definitionLens of
          typeDef@(Definition _ _ _ TIObject {}) -> traverse (customizeSelection typeDef) selSet
          typeDef@(Definition _ _ _ TIInterface {}) -> traverse (customizeSelection typeDef) selSet
          typeDef@(Definition _ _ _ TIUnion {}) -> traverse (customizeSelection typeDef) selSet
          _ -> pure $ (mempty,) <$> selSet
      let resultCustomizer =
            modifyFieldByName (fromMaybe fieldName' alias') $
              if fieldName' == $$(G.litName "__typename")
                then customizeTypeNameString (_rscCustomizeTypeName remoteSchemaCustomizer)
                else foldMap fst selSet'
      pure $ (resultCustomizer, G.Field alias' fieldName' args directives $ snd <$> selSet')

    customizeSelection :: Definition (TypeInfo 'Output) -> G.Selection G.NoFragments var -> n (ResultCustomizer, G.Selection G.NoFragments var)
    customizeSelection parentTypeDef = \case
      G.SelectionField fld@G.Field {..} ->
        withPath (++ [Key $ G.unName _fName]) $ do
          let parentTypeName = getName parentTypeDef
          fieldInfo <- findField _fName parentTypeName $ dInfo parentTypeDef
          second G.SelectionField <$> customizeField parentTypeName fieldInfo fld
      G.SelectionInlineFragment G.InlineFragment {..} -> do
        inlineFragmentType <-
          case _ifTypeCondition of
            Nothing -> pure parentTypeDef
            Just typeName -> findSubtype typeName parentTypeDef
        customizedSelectionSet <- traverse (customizeSelection inlineFragmentType) _ifSelectionSet
        pure $
          ( foldMap fst customizedSelectionSet,
            G.SelectionInlineFragment
              G.InlineFragment
                { _ifTypeCondition = remoteSchemaDecustomizeTypeName remoteSchemaCustomizer <$> _ifTypeCondition,
                  _ifSelectionSet = snd <$> customizedSelectionSet,
                  ..
                }
          )

    findField :: G.Name -> G.Name -> TypeInfo 'Output -> n P.FieldInfo
    findField fieldName parentTypeName parentTypeInfo =
      if fieldName == $$(G.litName "__typename") -- TODO can we avoid checking for __typename in two different places?
        then pure $ P.FieldInfo [] $ NonNullable $ TNamed $ mkDefinition (Typename stringScalar) Nothing TIScalar
        else do
          fields <- case parentTypeInfo of
            TIObject objectInfo -> pure $ oiFields objectInfo
            TIInterface interfaceInfo -> pure $ iiFields interfaceInfo
            _ -> parseError $ "Type " <> parentTypeName <<> " has no fields"
          fld <- find ((== fieldName) . dName) fields `onNothing` parseError ("field " <> fieldName <<> " not found in type: " <> squote parentTypeName)
          pure $ dInfo fld

    findSubtype :: G.Name -> Definition (TypeInfo 'Output) -> n (Definition (TypeInfo 'Output))
    findSubtype typeName parentTypeDef =
      if typeName == getName parentTypeDef
        then pure parentTypeDef
        else do
          possibleTypes <-
            case dInfo parentTypeDef of
              TIInterface interfaceInfo -> pure $ iiPossibleTypes interfaceInfo
              TIUnion unionInfo -> pure $ uiPossibleTypes unionInfo
              _ -> parseError $ "Type " <> getName parentTypeDef <<> " has no possible subtypes"
          fmap TIObject <$> find ((== typeName) . dName) possibleTypes
            `onNothing` parseError ("Type " <> typeName <<> " is not a subtype of " <>> getName parentTypeDef)

type MemoState = (HashMap G.Name ObjectInfo, HashMap G.Name InterfaceInfo, HashMap G.Name UnionInfo, HashMap G.Name InputObjectInfo)

evalMemoState :: Lazy.State MemoState a -> a
evalMemoState = flip Lazy.evalState (mempty, mempty, mempty, mempty)

-- | memo function used to "tie the knot" and preserve sharing in the customized type definitions
-- It would be nice if we could just re-use MonadSchema and memoizeOn, but the types there are too
-- parser-specific.
memo :: (MonadState s m, MonadFix m, Hashable k, Eq k) => Lens' s (HashMap k v) -> (k -> v -> m v) -> k -> v -> m v
memo lens f k v = do
  m <- use lens
  Map.lookup k m `onNothing` mdo
    -- Note: v' is added to the state _before_ it is produced
    lens %= Map.insert k v'
    v' <- f k v
    pure v'

customizeFieldDefinition ::
  forall m.
  (MonadState MemoState m, MonadFix m) =>
  RemoteSchemaCustomizer ->
  G.Name ->
  Definition P.FieldInfo ->
  m (Definition P.FieldInfo)
customizeFieldDefinition remoteSchemaCustomizer = customizeFieldDefinition'
  where
    customizeFieldDefinition' :: G.Name -> Definition P.FieldInfo -> m (Definition P.FieldInfo)
    customizeFieldDefinition' parentTypeName Definition {..} = do
      dInfo' <- customizeFieldInfo dInfo
      pure
        Definition
          { dName = remoteSchemaCustomizeFieldName remoteSchemaCustomizer parentTypeName dName,
            dInfo = dInfo',
            ..
          }

    customizeFieldInfo :: P.FieldInfo -> m P.FieldInfo
    customizeFieldInfo (P.FieldInfo args typ) =
      P.FieldInfo <$> traverse (traverse $ customizeInputFieldInfo) args <*> customizeType typ

    customizeTypeDefinition :: (G.Name -> b -> m b) -> Definition b -> m (Definition b)
    customizeTypeDefinition f Definition {..} = do
      dInfo' <- f dName dInfo
      pure
        Definition
          { dName = remoteSchemaCustomizeTypeName remoteSchemaCustomizer dName,
            dInfo = dInfo',
            ..
          }

    customizeType :: Type k -> m (Type k)
    customizeType = \case
      NonNullable nn -> NonNullable <$> customizeNonNullableType nn
      Nullable nn -> Nullable <$> customizeNonNullableType nn

    customizeNonNullableType :: NonNullableType k -> m (NonNullableType k)
    customizeNonNullableType = \case
      TList typ -> TList <$> customizeType typ
      TNamed definition -> TNamed <$> customizeTypeDefinition customizeTypeInfo definition

    customizeTypeInfo :: G.Name -> TypeInfo k -> m (TypeInfo k)
    customizeTypeInfo typeName = \case
      ti@TIScalar -> pure ti
      ti@TIEnum {} -> pure ti
      TIInputObject ioi -> TIInputObject <$> customizeInputObjectInfo typeName ioi
      TIObject oi -> TIObject <$> customizeObjectInfo typeName oi
      TIInterface ii -> TIInterface <$> customizeInterfaceInfo typeName ii
      TIUnion ui -> TIUnion <$> customizeUnionInfo typeName ui

    customizeInputFieldInfo :: InputFieldInfo -> m InputFieldInfo
    customizeInputFieldInfo = \case
      IFRequired nnType -> IFRequired <$> customizeNonNullableType nnType
      IFOptional typ value -> IFOptional <$> customizeType typ <*> pure value

    customizeObjectInfo :: G.Name -> ObjectInfo -> m ObjectInfo
    customizeObjectInfo = memo _1 $ \typeName ObjectInfo {..} -> do
      oiFields' <- traverse (customizeFieldDefinition' typeName) oiFields
      oiImplements' <- traverse (customizeTypeDefinition customizeInterfaceInfo) oiImplements
      pure
        ObjectInfo
          { oiFields = oiFields',
            oiImplements = oiImplements'
          }

    customizeInterfaceInfo :: G.Name -> InterfaceInfo -> m InterfaceInfo
    customizeInterfaceInfo = memo _2 $ \typeName InterfaceInfo {..} -> do
      iiFields' <- traverse (customizeFieldDefinition' typeName) iiFields
      iiPossibleTypes' <- traverse (customizeTypeDefinition customizeObjectInfo) iiPossibleTypes
      pure
        InterfaceInfo
          { iiFields = iiFields',
            iiPossibleTypes = iiPossibleTypes'
          }

    customizeUnionInfo :: G.Name -> UnionInfo -> m UnionInfo
    customizeUnionInfo = memo _3 $ \_typeName (UnionInfo possibleTypes) ->
      UnionInfo <$> traverse (customizeTypeDefinition customizeObjectInfo) possibleTypes

    customizeInputObjectInfo :: G.Name -> InputObjectInfo -> m InputObjectInfo
    customizeInputObjectInfo = memo _4 $ \_typeName (InputObjectInfo args) ->
      InputObjectInfo <$> traverse (traverse $ customizeInputFieldInfo) args
