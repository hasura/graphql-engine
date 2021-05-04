{-|
= Remote Schema Permissions Validation

This module parses the GraphQL IDL (Schema Document) that's provided by
the user for configuring permissions for remote schemas to a schema
introspection object, which is then used to construct the remote schema for
the particular role.

This module does two things essentially:

1. Checks if the given schema document is a subset of the upstream remote
   schema document. This is done by checking if all the objects, interfaces,
   unions, enums, scalars and input objects provided in the schema document
   exist in the upstream remote schema too. We validate the fields, directives
   and arguments too, wherever applicable.
2. Parse the `preset` directives (if any) on input object fields or argument fields.
   A `preset` directive is used to specify any preset argument on a field, it can be
   either a static value or session variable value. There is some validation done
   on preset directives. For example:
   - Preset directives can only be specified at
     `ARGUMENT_DEFINITION` or `INPUT_FIELD_DEFINITION`
   - A field expecting object cannot have a scalar/enum preset directive and vice versa.

   If a preset directive value is a session variable (like `x-hasura-*`), then it's
   considered to be a session variable value. In the case, the user wants to treat the
   session variable value literally, they can add the `static` key to the preset directive
   to indicate that the value provided should be considered literally. For example:

   `user(id: Int @preset(value: "x-hasura-user-id", static: true))

   In this case `x-hasura-user-id` will be considered literally.

For validation, we use the `MonadValidate` monad transformer to collect as many errors
as possible and then report all those errors at one go to the user.
-}
module Hasura.RQL.DDL.RemoteSchema.Permission (
    resolveRoleBasedRemoteSchema
  ) where

import           Hasura.Prelude

import           Control.Monad.Validate
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as S
import           Data.List.Extended            (duplicates, getDifference)
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import           Data.Text.Extended
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Schema.Remote
import           Hasura.RQL.Types              hiding (GraphQLType, defaultScalars)
import           Hasura.Server.Utils           (englishList, isSessionVariable)
import           Hasura.Session

data FieldDefinitionType
  = ObjectField
  | InterfaceField
  | EnumField
  deriving (Show, Eq)

instance ToTxt FieldDefinitionType where
  toTxt = \case
    ObjectField    -> "Object"
    InterfaceField -> "Interface"
    EnumField      -> "Enum"

data ArgumentDefinitionType
  = InputObjectArgument
  | DirectiveArgument
  deriving (Show, Eq)

instance ToTxt ArgumentDefinitionType where
  toTxt = \case
    InputObjectArgument -> "Input object"
    DirectiveArgument   -> "Directive"

data PresetInputTypeInfo
  = PresetScalar !G.Name
  | PresetEnum !G.Name ![G.EnumValue]
  | PresetInputObject ![G.InputValueDefinition]
  deriving (Show, Eq, Generic, Ord)

data GraphQLType
  = Enum
  | InputObject
  | Object
  | Interface
  | Union
  | Scalar
  | Directive
  | Field !FieldDefinitionType
  | Argument !ArgumentDefinitionType
  deriving (Show, Eq)

instance ToTxt GraphQLType where
  toTxt = \case
    Enum                         -> "Enum"
    InputObject                  -> "Input object"
    Object                       -> "Object"
    Interface                    -> "Interface"
    Union                        -> "Union"
    Scalar                       -> "Scalar"
    Directive                    -> "Directive"
    Field ObjectField            -> "Object field"
    Field InterfaceField         -> "Interface field"
    Field EnumField              -> "Enum field"
    Argument InputObjectArgument -> "Input object argument"
    Argument DirectiveArgument   -> "Directive Argument"

data RoleBasedSchemaValidationError
  = NonMatchingType !G.Name !GraphQLType !G.GType !G.GType
  -- ^ error to indicate that a type provided by the user
  -- differs from the corresponding type defined in the upstream
  -- remote schema
  | TypeDoesNotExist !GraphQLType !G.Name
  -- ^ error to indicate when a type definition doesn't exist
  -- in the upstream remote schema
  | NonMatchingDefaultValue !G.Name !G.Name !(Maybe (G.Value Void)) !(Maybe (G.Value Void))
  -- ^ error to indicate when the default value of an argument
  -- differs from the default value of the corresponding argument
  | NonExistingInputArgument !G.Name !G.Name
  -- ^ error to indicate when a given input argument doesn't exist
  -- in the corresponding upstream input object
  | MissingNonNullableArguments !G.Name !(NonEmpty G.Name)
  | NonExistingDirectiveArgument !G.Name !GraphQLType !G.Name !(NonEmpty G.Name)
  -- ^ error to indicate when a given directive argument
  -- doesn't exist in the corresponding upstream directive
  | NonExistingField !(FieldDefinitionType, G.Name) !G.Name
  -- ^ error to indicate when a given field doesn't exist in a field type (Object/Interface)
  | NonExistingUnionMemberTypes !G.Name !(NE.NonEmpty G.Name)
  -- ^ error to indicate when member types of an Union don't exist in the
  -- corresponding upstream union
  | CustomInterfacesNotAllowed !G.Name !(NE.NonEmpty G.Name)
  -- ^ error to indicate when an object is trying to implement an interface
  -- which exists in the schema document but the interface doesn't exist
  -- in the upstream remote.
  | ObjectImplementsNonExistingInterfaces !G.Name !(NE.NonEmpty G.Name)
  -- ^ error to indicate when object implements interfaces that don't exist
  | NonExistingEnumValues !G.Name !(NE.NonEmpty G.Name)
  -- ^ error to indicate enum values in an enum do not exist in the
  -- corresponding upstream enum
  | MultipleSchemaDefinitionsFound
  -- ^ error to indicate when the user provided schema contains more than
  -- one schema definition
  | MissingQueryRoot
  -- ^ error to indicate when the schema definition doesn't contain the
  -- query root.
  | DuplicateTypeNames !(NE.NonEmpty G.Name)
  | DuplicateDirectives !(GraphQLType, G.Name) !(NE.NonEmpty G.Name)
  | DuplicateFields !(FieldDefinitionType, G.Name) !(NE.NonEmpty G.Name)
  | DuplicateArguments !G.Name !(NE.NonEmpty G.Name)
  | DuplicateEnumValues !G.Name !(NE.NonEmpty G.Name)
  | InvalidPresetDirectiveLocation
  | MultiplePresetDirectives !(GraphQLType, G.Name)
  | NoPresetArgumentFound
  | InvalidPresetArgument !G.Name
  | ExpectedInputTypeButGotOutputType !G.Name
  | EnumValueNotFound !G.Name !G.Name
  | ExpectedEnumValue !G.Name !(G.Value Void)
  | KeyDoesNotExistInInputObject !G.Name !G.Name
  | ExpectedInputObject !G.Name !(G.Value Void)
  | ExpectedScalarValue !G.Name !(G.Value Void)
  | DisallowSessionVarForListType !G.Name
  | InvalidStaticValue
  | UnexpectedNonMatchingNames !G.Name !G.Name !GraphQLType
  -- ^ Error to indicate we're comparing non corresponding
  --   type definitions. Ideally, this error will never occur
  --   unless there's a programming error
  deriving (Show, Eq)

convertTypeDef :: G.TypeDefinition [G.Name] a -> G.TypeDefinition () a
convertTypeDef (G.TypeDefinitionInterface (G.InterfaceTypeDefinition desc name dirs flds _)) =
  G.TypeDefinitionInterface $ G.InterfaceTypeDefinition desc name dirs flds ()
convertTypeDef (G.TypeDefinitionScalar s) = G.TypeDefinitionScalar s
convertTypeDef (G.TypeDefinitionInputObject inpObj) = G.TypeDefinitionInputObject inpObj
convertTypeDef (G.TypeDefinitionEnum s) = G.TypeDefinitionEnum s
convertTypeDef (G.TypeDefinitionUnion s) = G.TypeDefinitionUnion s
convertTypeDef (G.TypeDefinitionObject s) = G.TypeDefinitionObject s

{- Note [Remote Schema Argument Presets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Remote schema argument presets are a way to inject values from static values or
from session variables during execution. Presets can be set using the `preset`
directive, the preset directive is defined in the following manner:

```
scalar PresetValue

directive @preset(
  value: PresetValue
) on INPUT_FIELD_DEFINITION | ARGUMENT_DEFINITION
```

When a preset directive is defined on an input type, the input type is removed
from the schema and the value is injected by the graphql-engine during the
execution.

There are two types of preset:

1. Static preset
----------------

Static preset is used to preset a static GraphQL value which will be injected
during the execution of the query. Static presets can be specified on all types
of input types i.e scalars, enums and input objects and lists of these types.
The preset value (if specified) will be validated against the type it's provided.
For example:

```
users(user_id: Int @preset(value: {user_id: 1}))
```

The above example will throw an error because the preset is attempting to preset
an input object value for a scalar (Int) type.

2. Session variable preset
--------------------------

Session variable preset is used to inject value from a session variable into the
graphql query during the execution. If the `value` argument of the preset directive
is in the format of the session variable i.e. `x-hasura-*` it will be treated as a
session variable preset. During the execution of a query, which has a session variable
preset set, the session variable's will be looked up and the value will be constructed
into a GraphQL variable. Check out `resolveRemoteVariable` for more details about how
the session variable presets get resolved.

At the time of writing this note, session variable presets can **only** be specified at
named types and only for scalar and enum types. This is done because currently there's
no good way to specify array or object values through session variables.
-}

{- Note [Remote Schema Permissions Architecture]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Remote schema permissions feature is designed in the following way:

1. An user can configure remote schema permissions for a particular role using
   the `add_remote_schema_permissions` API, note that this API will only work
   when remote schema permissions are enabled while starting the graphql-engine,
   which can be done either by the setting the server flag
   `--enable-remote-schema-permissions` or the env variable
   `HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS` to `true`. Check the module
   documentation of `Hasura.RQL.DDL.RemoteSchema.Permission` (this module) for
   more details about how the `add_remote_schema_permissions` API works.
2. The given schema document is parsed into an `IntrospectionResult` object,
3. The schema is built with the `IntrospectionResult` parsed in #2 for the said role.
   Check out the documentation in `argumentsParser` to know more about how the presets
   are handled.
4. For a remote schema query, the schema will return a `RemoteField` which
   contains unresolved session variables, the `RemoteField` is resolved using the
   `resolveRemoteField` function. The `resolveRemoteVariable` function contains more
   details about how the `RemoteVariable` is resolved.
5. After resolving the remote field, the remote server is queried with the resolved
   remote field.
-}

showRoleBasedSchemaValidationError :: RoleBasedSchemaValidationError -> Text
showRoleBasedSchemaValidationError = \case
  NonMatchingType fldName fldType expectedType providedType ->
    "expected type of " <> dquote fldName <> "(" <> dquote fldType <> ")" <>" to be " <>
    (G.showGT expectedType) <> " but received " <> (G.showGT providedType)
  TypeDoesNotExist graphQLType typeName ->
    graphQLType <<> ": " <> typeName <<> " does not exist in the upstream remote schema"
  NonMatchingDefaultValue inpObjName inpValName expectedVal providedVal ->
    "expected default value of input value: " <> inpValName <<> "of input object "
    <> inpObjName <<> " to be " <> defaultValueToText expectedVal <> " but received "
    <> defaultValueToText providedVal
  NonExistingInputArgument inpObjName inpArgName ->
    "input argument " <> inpArgName <<> " does not exist in the input object:" <>> inpObjName
  MissingNonNullableArguments fieldName nonNullableArgs ->
    "field: " <> fieldName <<> " expects the following non nullable arguments to "
    <> "be present: " <> englishList "and" (fmap dquote nonNullableArgs)
  NonExistingDirectiveArgument parentName parentType directiveName nonExistingArgs ->
    "the following directive argument(s) defined in the directive: "
    <> directiveName
    <<> " defined with the type name: "
    <> parentName <<> " of type "
    <> parentType <<> " do not exist in the corresponding upstream directive: "
    <> englishList "and" (fmap dquote nonExistingArgs)
  NonExistingField (fldDefnType, parentTypeName) providedName ->
    "field " <> providedName <<> " does not exist in the "
    <> fldDefnType <<> ": " <>> parentTypeName
  NonExistingUnionMemberTypes unionName nonExistingMembers ->
    "union " <> unionName <<> " contains members which do not exist in the members"
    <> " of the remote schema union :"
    <> englishList "and" (fmap dquote nonExistingMembers)
  CustomInterfacesNotAllowed objName customInterfaces ->
    "custom interfaces are not supported. " <> "Object" <> objName
    <<> " implements the following custom interfaces: "
    <> englishList "and" (fmap dquote customInterfaces)
  ObjectImplementsNonExistingInterfaces objName nonExistentInterfaces ->
    "object " <> objName <<> " is trying to implement the following interfaces"
    <> " that do not exist in the corresponding upstream remote object: "
    <> englishList "and" (fmap dquote nonExistentInterfaces)
  NonExistingEnumValues enumName nonExistentEnumVals ->
    "enum " <> enumName <<> " contains the following enum values that do not exist "
    <> "in the corresponding upstream remote enum: "
    <> englishList "and" (fmap dquote nonExistentEnumVals)
  MissingQueryRoot -> "query root does not exist in the schema definition"
  MultipleSchemaDefinitionsFound -> "multiple schema definitions found"
  DuplicateTypeNames typeNames ->
    "duplicate type names found: "
    <> englishList "and" ( fmap dquote typeNames)
  DuplicateDirectives (parentType, parentName) directiveNames ->
    "duplicate directives: " <> englishList "and" (fmap dquote directiveNames)
    <> "found in the " <> parentType <<> " " <>> parentName
  DuplicateFields (parentType, parentName) fieldNames ->
    "duplicate fields: " <> englishList "and" (fmap dquote fieldNames)
    <> "found in the " <> parentType <<> " " <>> parentName
  DuplicateArguments fieldName args ->
    "duplicate arguments: "
    <> englishList "and" (fmap dquote args)
    <> "found in the field: " <>> fieldName
  DuplicateEnumValues enumName enumValues ->
    "duplicate enum values: " <> englishList "and" (fmap dquote enumValues)
    <> " found in the " <> enumName <<> " enum"
  InvalidPresetDirectiveLocation ->
    "Preset directives can be defined only on INPUT_FIELD_DEFINITION or ARGUMENT_DEFINITION"
  MultiplePresetDirectives (parentType, parentName) ->
    "found multiple preset directives at " <> parentType <<> " " <>> parentName
  NoPresetArgumentFound -> "no arguments found in the preset directive"
  InvalidPresetArgument argName ->
    "preset argument \"value\" not found at " <>> argName
  ExpectedInputTypeButGotOutputType typeName -> "expected " <> typeName <<> " to be an input type, but it's an output type"
  EnumValueNotFound enumName enumValue -> enumValue <<> " not found in the enum: " <>> enumName
  ExpectedEnumValue typeName presetValue ->
    "expected preset value " <> presetValue
    <<> " of type " <> typeName <<> " to be an enum value"
  ExpectedScalarValue typeName presetValue ->
    "expected preset value " <> presetValue
    <<> " of type " <> typeName <<> " to be a scalar value"
  ExpectedInputObject typeName presetValue ->
    "expected preset value " <> presetValue
    <<> " of type " <> typeName <<> " to be an input object value"
  KeyDoesNotExistInInputObject key' inpObjTypeName ->
    key' <<> " does not exist in the input object " <>> inpObjTypeName
  DisallowSessionVarForListType name ->
    "illegal preset value at " <> name <<> ". Session arguments can only be set for singleton values"
  InvalidStaticValue ->
    "expected preset static value to be a Boolean value"
  UnexpectedNonMatchingNames providedName upstreamName gType ->
    "unexpected: trying to compare " <> gType <<> " with name " <> providedName <<>
    " with " <>> upstreamName
  where
    defaultValueToText = \case
      Just defaultValue -> toTxt defaultValue
      Nothing           -> ""

presetValueScalar :: G.ScalarTypeDefinition
presetValueScalar = G.ScalarTypeDefinition Nothing $$(G.litName "PresetValue") mempty

presetDirectiveDefn :: G.DirectiveDefinition G.InputValueDefinition
presetDirectiveDefn =
  G.DirectiveDefinition Nothing $$(G.litName "preset") [directiveArg] directiveLocations
  where
    gType              = G.TypeNamed (G.Nullability False) $ G._stdName presetValueScalar

    directiveLocations = map G.DLTypeSystem [G.TSDLARGUMENT_DEFINITION, G.TSDLINPUT_FIELD_DEFINITION]

    directiveArg       = G.InputValueDefinition Nothing $$(G.litName "value") gType Nothing mempty

presetDirectiveName :: G.Name
presetDirectiveName = $$(G.litName "preset")

lookupInputType
  :: G.SchemaDocument
  -> G.Name
  -> Maybe PresetInputTypeInfo
lookupInputType (G.SchemaDocument types) name = go types
  where
    go :: [G.TypeSystemDefinition] -> Maybe PresetInputTypeInfo
    go (tp:tps) =
      case tp of
        G.TypeSystemDefinitionSchema _ -> go tps
        G.TypeSystemDefinitionType typeDef ->
          case typeDef of
            G.TypeDefinitionScalar (G.ScalarTypeDefinition _ scalarName _) ->
              if | name == scalarName -> Just $ PresetScalar scalarName
                 | otherwise          -> go tps
            G.TypeDefinitionEnum (G.EnumTypeDefinition _ enumName _ vals) ->
              if | name == enumName -> Just $ PresetEnum enumName $ map G._evdName vals
                 | otherwise        -> go tps
            G.TypeDefinitionInputObject (G.InputObjectTypeDefinition _ inpObjName _ vals) ->
              if | name == inpObjName -> Just $ PresetInputObject vals
                 | otherwise          -> go tps
            _ -> go tps
    go [] = Nothing

-- | `parsePresetValue` constructs a GraphQL value when an input value definition
--    contains a preset with it. This function checks if the given preset value
--    is a legal value to the field that's specified it. For example: A scalar input
--    value cannot contain an input object value. When the preset value is a session
--    variable, we treat it as a session variable whose value will be resolved while
--    the query is executed. In the case of session variables preset, we make the GraphQL
--    value as a Variable value and during the execution we resolve all these
--    "session variable" variable(s) and then query the remote server.
parsePresetValue
  :: forall m
   . ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => G.GType
  -> G.Name
  -> Bool
  -> G.Value Void
  -> m (G.Value RemoteSchemaVariable)
parsePresetValue gType varName isStatic value = do
  schemaDoc <- ask
  case gType of
    G.TypeNamed _ typeName ->
      case (lookupInputType schemaDoc typeName) of
        Nothing -> refute $ pure $ ExpectedInputTypeButGotOutputType typeName
        Just (PresetScalar scalarTypeName) ->
          case value of
            G.VEnum _ -> refute $ pure $ ExpectedScalarValue typeName value
            G.VString t ->
              case (isSessionVariable t && (not isStatic)) of
                True ->
                  pure $
                    G.VVariable $
                    SessionPresetVariable (mkSessionVariable t) scalarTypeName $
                    SessionArgumentPresetScalar
                False -> pure $ G.VString t
            G.VList _ -> refute $ pure $ ExpectedScalarValue typeName value
            G.VObject _ -> refute $ pure $ ExpectedScalarValue typeName value
            v -> pure $ G.literal v
        Just (PresetEnum enumTypeName enumVals) ->
          case value of
            enumVal@(G.VEnum e) ->
              case e `elem` enumVals of
                True  -> pure $ G.literal enumVal
                False -> refute $ pure $ EnumValueNotFound typeName $ G.unEnumValue e
            G.VString t ->
              case isSessionVariable t of
                True ->
                  pure $
                    G.VVariable $
                    SessionPresetVariable (mkSessionVariable t) enumTypeName $
                    SessionArgumentPresetEnum $
                    S.fromList enumVals
                False -> refute $ pure $ ExpectedEnumValue typeName value
            _ -> refute $ pure $ ExpectedEnumValue typeName value
        Just (PresetInputObject inputValueDefinitions) ->
          let inpValsMap = mapFromL G._ivdName inputValueDefinitions
              parseInputObjectField k val = do
                inpVal <- onNothing (Map.lookup k inpValsMap) (refute $ pure $ KeyDoesNotExistInInputObject k typeName)
                parsePresetValue (G._ivdType inpVal) k isStatic val
          in
          case value of
            G.VObject obj ->
              G.VObject <$> Map.traverseWithKey parseInputObjectField obj
            _ -> refute $ pure $ ExpectedInputObject typeName value
    G.TypeList _ gType' ->
      case value of
        G.VList lst -> G.VList <$> traverse (parsePresetValue gType' varName isStatic) lst
        -- The below is valid because singleton GraphQL values can be "upgraded"
        -- to array types. For ex: An `Int` value can be provided as input to
        -- a type `[Int]` or `[[Int]]`
        s'@(G.VString s) ->
          case isSessionVariable s of
            True  -> refute $ pure $ DisallowSessionVarForListType varName
            False -> parsePresetValue gType' varName isStatic s'
        v -> parsePresetValue gType' varName isStatic v

parsePresetDirective
  :: forall m
  .  ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => G.GType
  -> G.Name
  -> G.Directive Void
  -> m (G.Value RemoteSchemaVariable)
parsePresetDirective gType parentArgName (G.Directive name args) = do
  if | Map.null args -> refute $ pure $ NoPresetArgumentFound
     | otherwise -> do
         val <-
           onNothing (Map.lookup $$(G.litName "value") args) $
           refute $ pure $ InvalidPresetArgument parentArgName
         isStatic <-
           case (Map.lookup $$(G.litName "static") args) of
             Nothing               -> pure False
             (Just (G.VBoolean b)) -> pure b
             _                     -> refute $ pure $ InvalidStaticValue
         parsePresetValue gType parentArgName isStatic val

-- | validateDirective checks if the arguments of a given directive
--   is a subset of the corresponding upstream directive arguments
--   *NOTE*: This function assumes that the `providedDirective` and the
--   `upstreamDirective` have the same name.
validateDirective
  :: MonadValidate [RoleBasedSchemaValidationError] m
  => G.Directive a -- ^ provided directive
  -> G.Directive a -- ^ upstream directive
  -> (GraphQLType, G.Name) -- ^ parent type and name
  -> m ()
validateDirective providedDirective upstreamDirective (parentType, parentTypeName) = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName Directive
  onJust (NE.nonEmpty $ Map.keys argsDiff) $ \argNames ->
    dispute $ pure $
      NonExistingDirectiveArgument parentTypeName parentType providedName argNames
  where
    argsDiff              = Map.difference providedDirectiveArgs upstreamDirectiveArgs

    G.Directive providedName providedDirectiveArgs = providedDirective
    G.Directive upstreamName upstreamDirectiveArgs = upstreamDirective

-- | validateDirectives checks if the `providedDirectives`
--   are a subset of `upstreamDirectives` and then validate
--   each of the directives by calling the `validateDirective`
validateDirectives
  :: MonadValidate [RoleBasedSchemaValidationError] m
  => [G.Directive a]
  -> [G.Directive a]
  -> G.TypeSystemDirectiveLocation
  -> (GraphQLType, G.Name)
  -> m (Maybe (G.Directive a))
validateDirectives providedDirectives upstreamDirectives directiveLocation parentType = do
  onJust (NE.nonEmpty $ S.toList $ duplicates $ map G._dName nonPresetDirectives) $ \dups -> do
    refute $ pure $ DuplicateDirectives parentType dups
  for_ nonPresetDirectives $ \dir -> do
    let directiveName = G._dName dir
    upstreamDir <-
      onNothing (Map.lookup directiveName upstreamDirectivesMap) $
        refute $ pure $ TypeDoesNotExist Directive directiveName
    validateDirective dir upstreamDir parentType
  case presetDirectives of
    [] -> pure Nothing
    [presetDirective] -> do
      case directiveLocation of
        G.TSDLINPUT_FIELD_DEFINITION -> pure ()
        G.TSDLARGUMENT_DEFINITION    -> pure ()
        _                            -> dispute $ pure $ InvalidPresetDirectiveLocation
      pure $ Just presetDirective
    _ ->
      refute $ pure $ MultiplePresetDirectives parentType
  where
    upstreamDirectivesMap = mapFromL G._dName upstreamDirectives

    presetFilterFn = (== $$(G.litName "preset")) . G._dName

    presetDirectives = filter presetFilterFn providedDirectives

    nonPresetDirectives = filter (not . presetFilterFn) providedDirectives

-- |  `validateEnumTypeDefinition` checks the validity of an enum definition
-- provided by the user against the corresponding upstream enum.
-- The function does the following things:
-- 1. Validates the directives (if any)
-- 2. For each enum provided, check if the enum values are a subset of
--    the enum values of the corresponding upstream enum
-- *NOTE*: This function assumes that the `providedEnum` and the `upstreamEnum`
-- have the same name.
validateEnumTypeDefinition
  :: ( MonadValidate [RoleBasedSchemaValidationError] m)
  => G.EnumTypeDefinition -- ^ provided enum type definition
  -> G.EnumTypeDefinition -- ^ upstream enum type definition
  -> m G.EnumTypeDefinition
validateEnumTypeDefinition providedEnum upstreamEnum = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName Enum
  validateDirectives providedDirectives upstreamDirectives G.TSDLENUM $ (Enum, providedName)
  onJust (NE.nonEmpty $ S.toList $ duplicates providedEnumValNames) $ \dups -> do
    refute $ pure $ DuplicateEnumValues providedName dups
  onJust (NE.nonEmpty $ S.toList fieldsDifference) $ \nonExistingEnumVals ->
    dispute $ pure $ NonExistingEnumValues providedName nonExistingEnumVals
  pure providedEnum
  where
    G.EnumTypeDefinition _ providedName providedDirectives providedValueDefns = providedEnum

    G.EnumTypeDefinition _ upstreamName upstreamDirectives upstreamValueDefns = upstreamEnum

    providedEnumValNames   = map (G.unEnumValue . G._evdName) $ providedValueDefns

    upstreamEnumValNames   = map (G.unEnumValue . G._evdName) $ upstreamValueDefns

    fieldsDifference       = getDifference providedEnumValNames upstreamEnumValNames

-- | `validateInputValueDefinition` validates a given input value definition
--   , against the corresponding upstream input value definition. Two things
--   are validated to do the same, the type and the default value of the
--   input value definitions should be equal.
validateInputValueDefinition
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => G.InputValueDefinition
  -> G.InputValueDefinition
  -> G.Name
  -> m RemoteSchemaInputValueDefinition
validateInputValueDefinition providedDefn upstreamDefn inputObjectName = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName (Argument InputObjectArgument)
  presetDirective <-
    validateDirectives providedDirectives upstreamDirectives G.TSDLINPUT_FIELD_DEFINITION
     $ (Argument InputObjectArgument, inputObjectName)
  when (providedType /= upstreamType) $
    dispute $ pure $
      NonMatchingType providedName (Argument InputObjectArgument) upstreamType providedType
  when (providedDefaultValue /= upstreamDefaultValue) $
    dispute $ pure $
      NonMatchingDefaultValue inputObjectName providedName
                              upstreamDefaultValue providedDefaultValue
  presetArguments <- for presetDirective $ parsePresetDirective providedType providedName
  pure $ RemoteSchemaInputValueDefinition providedDefn presetArguments
  where
    G.InputValueDefinition _ providedName providedType providedDefaultValue providedDirectives  = providedDefn
    G.InputValueDefinition _ upstreamName upstreamType upstreamDefaultValue upstreamDirectives  = upstreamDefn

-- | `validateArguments` validates the provided arguments against the corresponding
--    upstream remote schema arguments.
validateArguments
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => (G.ArgumentsDefinition G.InputValueDefinition)
  -> (G.ArgumentsDefinition RemoteSchemaInputValueDefinition)
  -> G.Name
  -> m [RemoteSchemaInputValueDefinition]
validateArguments providedArgs upstreamArgs parentTypeName = do
  onJust (NE.nonEmpty $ S.toList $ duplicates $ map G._ivdName providedArgs) $ \dups -> do
    refute $ pure $ DuplicateArguments parentTypeName dups
  let argsDiff = getDifference nonNullableUpstreamArgs nonNullableProvidedArgs
  onJust (NE.nonEmpty $ S.toList argsDiff) $ \nonNullableArgs -> do
    refute $ pure $ MissingNonNullableArguments parentTypeName nonNullableArgs
  for providedArgs $ \providedArg@(G.InputValueDefinition _ name _ _ _) -> do
    upstreamArg <-
      onNothing (Map.lookup name upstreamArgsMap) $
        refute $ pure $ NonExistingInputArgument parentTypeName name
    validateInputValueDefinition providedArg upstreamArg parentTypeName
  where
    upstreamArgsMap = mapFromL G._ivdName $  map _rsitdDefinition upstreamArgs

    nonNullableUpstreamArgs = map G._ivdName $ filter (not . G.isNullable . G._ivdType) $ map _rsitdDefinition upstreamArgs

    nonNullableProvidedArgs = map G._ivdName $ filter (not . G.isNullable . G._ivdType) providedArgs

validateInputObjectTypeDefinition
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => G.InputObjectTypeDefinition G.InputValueDefinition
  -> G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition
  -> m (G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition)
validateInputObjectTypeDefinition providedInputObj upstreamInputObj = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName InputObject
  validateDirectives providedDirectives upstreamDirectives G.TSDLINPUT_OBJECT $ (InputObject, providedName)
  args <- validateArguments providedArgs upstreamArgs $ providedName
  pure $ providedInputObj { G._iotdValueDefinitions = args }
  where
    G.InputObjectTypeDefinition _ providedName providedDirectives providedArgs = providedInputObj

    G.InputObjectTypeDefinition _ upstreamName upstreamDirectives upstreamArgs = upstreamInputObj

validateFieldDefinition
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => (G.FieldDefinition G.InputValueDefinition)
  -> (G.FieldDefinition RemoteSchemaInputValueDefinition)
  -> (FieldDefinitionType, G.Name)
  -> m (G.FieldDefinition RemoteSchemaInputValueDefinition)
validateFieldDefinition providedFieldDefinition upstreamFieldDefinition (parentType, parentTypeName) = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName (Field parentType)
  validateDirectives providedDirectives upstreamDirectives G.TSDLFIELD_DEFINITION $ (Field parentType, parentTypeName)
  when (providedType /= upstreamType) $
    dispute $ pure $ NonMatchingType providedName (Field parentType) upstreamType providedType
  args <- validateArguments providedArgs upstreamArgs $ providedName
  pure $ providedFieldDefinition { G._fldArgumentsDefinition = args }
  where
    G.FieldDefinition _ providedName providedArgs providedType providedDirectives = providedFieldDefinition

    G.FieldDefinition _ upstreamName upstreamArgs upstreamType upstreamDirectives = upstreamFieldDefinition

validateFieldDefinitions
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => [(G.FieldDefinition G.InputValueDefinition)]
  -> [(G.FieldDefinition RemoteSchemaInputValueDefinition)]
  -> (FieldDefinitionType, G.Name) -- ^ parent type and name
  -> m [(G.FieldDefinition RemoteSchemaInputValueDefinition)]
validateFieldDefinitions providedFldDefnitions upstreamFldDefinitions parentType = do
  onJust (NE.nonEmpty $ S.toList $ duplicates $ map G._fldName providedFldDefnitions) $ \dups -> do
    refute $ pure $ DuplicateFields parentType dups
  for providedFldDefnitions $ \fldDefn@(G.FieldDefinition _ name _ _ _) -> do
    upstreamFldDefn <-
      onNothing (Map.lookup name upstreamFldDefinitionsMap) $
        refute $ pure $ NonExistingField parentType name
    validateFieldDefinition fldDefn upstreamFldDefn parentType
  where
    upstreamFldDefinitionsMap = mapFromL G._fldName upstreamFldDefinitions

validateInterfaceDefinition
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => G.InterfaceTypeDefinition () G.InputValueDefinition
  -> G.InterfaceTypeDefinition [G.Name] RemoteSchemaInputValueDefinition
  -> m (G.InterfaceTypeDefinition () RemoteSchemaInputValueDefinition)
validateInterfaceDefinition providedInterfaceDefn upstreamInterfaceDefn = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName Interface
  validateDirectives providedDirectives upstreamDirectives G.TSDLINTERFACE $ (Interface, providedName)
  fieldDefinitions <- validateFieldDefinitions providedFieldDefns upstreamFieldDefns $ (InterfaceField, providedName)
  pure $ providedInterfaceDefn { G._itdFieldsDefinition = fieldDefinitions }
  where
    G.InterfaceTypeDefinition _ providedName providedDirectives providedFieldDefns _ = providedInterfaceDefn

    G.InterfaceTypeDefinition _ upstreamName upstreamDirectives upstreamFieldDefns _ = upstreamInterfaceDefn

validateScalarDefinition
  :: MonadValidate [RoleBasedSchemaValidationError] m
  => G.ScalarTypeDefinition
  -> G.ScalarTypeDefinition
  -> m G.ScalarTypeDefinition
validateScalarDefinition providedScalar upstreamScalar = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName Scalar
  void $ validateDirectives providedDirectives upstreamDirectives G.TSDLSCALAR $ (Scalar, providedName)
  pure providedScalar
  where
    G.ScalarTypeDefinition _ providedName providedDirectives = providedScalar

    G.ScalarTypeDefinition _ upstreamName upstreamDirectives = upstreamScalar

validateUnionDefinition
  :: MonadValidate [RoleBasedSchemaValidationError] m
  => G.UnionTypeDefinition
  -> G.UnionTypeDefinition
  -> m G.UnionTypeDefinition
validateUnionDefinition providedUnion upstreamUnion = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName Union
  void $ validateDirectives providedDirectives upstreamDirectives G.TSDLUNION $ (Union, providedName)
  onJust (NE.nonEmpty $ S.toList memberTypesDiff) $ \nonExistingMembers ->
    refute $ pure $ NonExistingUnionMemberTypes providedName nonExistingMembers
  pure providedUnion
  where
    G.UnionTypeDefinition _ providedName providedDirectives providedMemberTypes = providedUnion

    G.UnionTypeDefinition _ upstreamName upstreamDirectives upstreamMemberTypes = upstreamUnion

    memberTypesDiff = getDifference providedMemberTypes upstreamMemberTypes

validateObjectDefinition
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => G.ObjectTypeDefinition G.InputValueDefinition
  -> G.ObjectTypeDefinition RemoteSchemaInputValueDefinition
  -> S.HashSet G.Name -- ^ Interfaces declared by in the role-based schema
  -> m (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
validateObjectDefinition providedObj upstreamObj interfacesDeclared = do
  when (providedName /= upstreamName) $
    dispute $ pure $
      UnexpectedNonMatchingNames providedName upstreamName Object
  validateDirectives providedDirectives upstreamDirectives G.TSDLOBJECT $ (Object, providedName)
  onJust (NE.nonEmpty $ S.toList customInterfaces) $ \ifaces ->
    dispute $ pure $ CustomInterfacesNotAllowed providedName ifaces
  onJust (NE.nonEmpty nonExistingInterfaces) $ \ifaces ->
    dispute $ pure $ ObjectImplementsNonExistingInterfaces providedName ifaces
  fieldDefinitions <-
    validateFieldDefinitions providedFldDefnitions upstreamFldDefnitions $ (ObjectField, providedName)
  pure $ providedObj { G._otdFieldsDefinition = fieldDefinitions }
  where
    G.ObjectTypeDefinition _ providedName
       providedIfaces providedDirectives providedFldDefnitions = providedObj

    G.ObjectTypeDefinition _ upstreamName
       upstreamIfaces upstreamDirectives upstreamFldDefnitions = upstreamObj

    interfacesDiff = getDifference providedIfaces upstreamIfaces

    providedIfacesSet = S.fromList providedIfaces

    customInterfaces = S.intersection interfacesDiff interfacesDeclared

    nonExistingInterfaces = S.toList $ S.difference interfacesDiff providedIfacesSet

-- | helper function to validate the schema definitions mentioned in the schema
-- document.
validateSchemaDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.SchemaDefinition]
  -> m (Maybe G.Name, Maybe G.Name, Maybe G.Name)
validateSchemaDefinitions [] = pure $ (Nothing, Nothing, Nothing)
validateSchemaDefinitions [schemaDefn] = do
  let G.SchemaDefinition _ rootOpsTypes = schemaDefn
      rootOpsTypesMap = mapFromL G._rotdOperationType rootOpsTypes
      mQueryRootName = G._rotdOperationTypeType <$> Map.lookup G.OperationTypeQuery rootOpsTypesMap
      mMutationRootName = G._rotdOperationTypeType <$> Map.lookup G.OperationTypeMutation rootOpsTypesMap
      mSubscriptionRootName = G._rotdOperationTypeType <$> Map.lookup G.OperationTypeSubscription rootOpsTypesMap
  pure (mQueryRootName, mMutationRootName, mSubscriptionRootName)
validateSchemaDefinitions _ = refute $ pure $ MultipleSchemaDefinitionsFound

-- | Construction of the `possibleTypes` map for interfaces, while parsing the
-- user provided Schema document, it doesn't include the `possibleTypes`, so
-- constructing here, manually.
createPossibleTypesMap :: [(G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)] -> HashMap G.Name [G.Name]
createPossibleTypesMap objectDefinitions = do
  Map.fromListWith (<>) $ do
    objectDefinition <- objectDefinitions
    let objectName = G._otdName objectDefinition
    interface <- G._otdImplementsInterfaces objectDefinition
    pure (interface, [objectName])

partitionTypeSystemDefinitions
  :: [G.TypeSystemDefinition]
  -> ([G.SchemaDefinition], [G.TypeDefinition () G.InputValueDefinition])
partitionTypeSystemDefinitions  = foldr f ([], [])
  where
    f d (schemaDefinitions, typeDefinitions) = case d of
      G.TypeSystemDefinitionSchema schemaDefinition -> ((schemaDefinition: schemaDefinitions), typeDefinitions)
      G.TypeSystemDefinitionType   typeDefinition   -> (schemaDefinitions, (typeDefinition: typeDefinitions))

-- | getSchemaDocIntrospection converts the `PartitionedTypeDefinitions` to
-- `IntrospectionResult` because the function `buildRemoteParser` function which
-- builds the remote schema parsers accepts an `IntrospectionResult`. The
-- conversion involves converting `G.TypeDefinition ()` to `G.TypeDefinition
-- [G.Name]`. The `[G.Name]` here being the list of object names that an
-- interface implements. This is needed to be done here by-hand because while
-- specifying the `SchemaDocument` through the GraphQL DSL, it doesn't include
-- the `possibleTypes` along with an object.
getSchemaDocIntrospection
  :: [G.TypeDefinition () RemoteSchemaInputValueDefinition]
  -> (Maybe G.Name, Maybe G.Name, Maybe G.Name)
  -> IntrospectionResult
getSchemaDocIntrospection providedTypeDefns (queryRoot, mutationRoot, subscriptionRoot) =
  let objects = flip mapMaybe providedTypeDefns $ \case
                  G.TypeDefinitionObject obj -> Just obj
                  _                          -> Nothing
      possibleTypesMap = createPossibleTypesMap objects
      modifiedTypeDefns = do
        providedType <- providedTypeDefns
        case providedType of
          G.TypeDefinitionInterface interface@(G.InterfaceTypeDefinition _ name _ _ _) ->
            pure $
              G.TypeDefinitionInterface $
              interface { G._itdPossibleTypes = concat $ maybeToList (Map.lookup name possibleTypesMap) }
          G.TypeDefinitionScalar scalar -> pure $ G.TypeDefinitionScalar scalar
          G.TypeDefinitionEnum enum -> pure $ G.TypeDefinitionEnum enum
          G.TypeDefinitionObject obj -> pure $ G.TypeDefinitionObject obj
          G.TypeDefinitionUnion union' -> pure $ G.TypeDefinitionUnion union'
          G.TypeDefinitionInputObject inpObj -> pure $ G.TypeDefinitionInputObject inpObj
      remoteSchemaIntrospection = RemoteSchemaIntrospection modifiedTypeDefns
  in IntrospectionResult remoteSchemaIntrospection (fromMaybe $$(G.litName "Query") queryRoot) mutationRoot subscriptionRoot

-- | validateRemoteSchema accepts two arguments, the `SchemaDocument` of
--   the role-based schema, that is provided by the user and the `SchemaIntrospection`
--   of the upstream remote schema. This function, in turn calls the other validation
--   functions for scalars, enums, unions, interfaces,input objects and objects.
validateRemoteSchema
  :: ( MonadValidate [RoleBasedSchemaValidationError] m
     , MonadReader G.SchemaDocument m
     )
  => RemoteSchemaIntrospection
  -> m IntrospectionResult
validateRemoteSchema upstreamRemoteSchemaIntrospection = do
  G.SchemaDocument providedTypeSystemDefinitions <- ask
  let (providedSchemaDefinitions, providedTypeDefinitions) =
        partitionTypeSystemDefinitions providedTypeSystemDefinitions
      duplicateTypesList = S.toList $ duplicates (getTypeName <$> providedTypeDefinitions)
  onJust (NE.nonEmpty duplicateTypesList) $ \duplicateTypeNames ->
    refute $ pure $ DuplicateTypeNames duplicateTypeNames
  rootTypeNames <- validateSchemaDefinitions providedSchemaDefinitions
  let providedInterfacesTypes =
        S.fromList $
        flip mapMaybe providedTypeDefinitions $ \case
          G.TypeDefinitionInterface interface -> Just $ G._itdName interface
          _                                   -> Nothing
  validatedTypeDefinitions <-
    for providedTypeDefinitions $ \case
      G.TypeDefinitionScalar providedScalarTypeDefn -> do
        let nameTxt = G.unName $ G._stdName providedScalarTypeDefn
        case nameTxt `elem` ["ID", "Int", "Float", "Boolean", "String"] of
          True -> pure $ G.TypeDefinitionScalar providedScalarTypeDefn
          False -> do
            upstreamScalarTypeDefn <-
              lookupScalar upstreamRemoteSchemaIntrospection (G._stdName providedScalarTypeDefn)
              `onNothing`
              typeNotFound Scalar (G._stdName providedScalarTypeDefn)
            G.TypeDefinitionScalar <$> validateScalarDefinition providedScalarTypeDefn upstreamScalarTypeDefn
      G.TypeDefinitionInterface providedInterfaceTypeDefn -> do
        upstreamInterfaceTypeDefn <-
          lookupInterface upstreamRemoteSchemaIntrospection (G._itdName providedInterfaceTypeDefn)
          `onNothing`
          typeNotFound Interface (G._itdName providedInterfaceTypeDefn)
        G.TypeDefinitionInterface <$> validateInterfaceDefinition providedInterfaceTypeDefn upstreamInterfaceTypeDefn
      G.TypeDefinitionObject providedObjectTypeDefn -> do
        upstreamObjectTypeDefn <-
          lookupObject upstreamRemoteSchemaIntrospection (G._otdName providedObjectTypeDefn)
          `onNothing`
          typeNotFound Object (G._otdName providedObjectTypeDefn)
        G.TypeDefinitionObject
          <$>
          validateObjectDefinition providedObjectTypeDefn upstreamObjectTypeDefn providedInterfacesTypes
      G.TypeDefinitionUnion providedUnionTypeDefn -> do
        upstreamUnionTypeDefn <-
          lookupUnion upstreamRemoteSchemaIntrospection (G._utdName providedUnionTypeDefn)
          `onNothing`
          typeNotFound Union (G._utdName providedUnionTypeDefn)
        G.TypeDefinitionUnion <$> validateUnionDefinition providedUnionTypeDefn upstreamUnionTypeDefn
      G.TypeDefinitionEnum providedEnumTypeDefn -> do
        upstreamEnumTypeDefn <-
          lookupEnum upstreamRemoteSchemaIntrospection (G._etdName providedEnumTypeDefn)
          `onNothing`
          typeNotFound Enum (G._etdName providedEnumTypeDefn)
        G.TypeDefinitionEnum <$> validateEnumTypeDefinition providedEnumTypeDefn upstreamEnumTypeDefn
      G.TypeDefinitionInputObject providedInputObjectTypeDefn -> do
        upstreamInputObjectTypeDefn <-
          lookupInputObject upstreamRemoteSchemaIntrospection (G._iotdName providedInputObjectTypeDefn)
          `onNothing`
          typeNotFound InputObject (G._iotdName providedInputObjectTypeDefn)
        G.TypeDefinitionInputObject
          <$> validateInputObjectTypeDefinition providedInputObjectTypeDefn upstreamInputObjectTypeDefn
  pure $ getSchemaDocIntrospection validatedTypeDefinitions rootTypeNames
  where
    getTypeName = \case
      G.TypeDefinitionScalar scalar      -> G._stdName scalar
      G.TypeDefinitionEnum enum          -> G._etdName enum
      G.TypeDefinitionObject obj         -> G._otdName obj
      G.TypeDefinitionUnion union'       -> G._utdName union'
      G.TypeDefinitionInterface iface    -> G._itdName iface
      G.TypeDefinitionInputObject inpObj -> G._iotdName inpObj

    typeNotFound gType name = refute (pure $ TypeDoesNotExist gType name)

resolveRoleBasedRemoteSchema
  :: MonadError QErr m
  => G.SchemaDocument
  -> RemoteSchemaCtx
  -> m (IntrospectionResult, [SchemaDependency])
resolveRoleBasedRemoteSchema (G.SchemaDocument providedTypeDefns) upstreamRemoteCtx = do
  let providedSchemaDocWithDefaultScalars =
        G.SchemaDocument $
        providedTypeDefns <> (map (G.TypeSystemDefinitionType . G.TypeDefinitionScalar) defaultScalars)
  introspectionRes <-
    flip onLeft (throw400 ValidationFailed . showErrors)
    =<< runValidateT
         (flip runReaderT providedSchemaDocWithDefaultScalars
         $ validateRemoteSchema $ irDoc $ _rscIntro upstreamRemoteCtx)
  pure (introspectionRes, [schemaDependency])
  where
    showErrors :: [RoleBasedSchemaValidationError] -> Text
    showErrors errors =
      "validation for the given role-based schema failed " <> reasonsMessage
      where
        reasonsMessage = case errors of
          [singleError] -> "because " <> showRoleBasedSchemaValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
             (map ((" • " <>) . showRoleBasedSchemaValidationError) errors)

    schemaDependency = SchemaDependency (SORemoteSchema $ _rscName upstreamRemoteCtx) DRRemoteSchema

    defaultScalars = map (\n -> G.ScalarTypeDefinition Nothing n [])
                         $ [intScalar, floatScalar, stringScalar, boolScalar, idScalar]
