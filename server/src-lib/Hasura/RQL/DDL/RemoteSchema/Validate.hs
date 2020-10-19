module Hasura.RQL.DDL.RemoteSchema.Validate (
    resolveRoleBasedRemoteSchema
  , partitionTypeDefinition
  ) where

import           Control.Monad.Validate

import           Hasura.Prelude
import           Hasura.SQL.Types
import           Hasura.RQL.Types              hiding (GraphQLType, defaultScalars)
import           Hasura.Server.Utils           (englishList, duplicates, isSessionVariable)

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Data.HashSet                  as S
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T

import           Hasura.Session

data FieldDefinitionType
  = ObjectField
  | InterfaceField
  | EnumField
  deriving (Show, Eq)

instance DQuote FieldDefinitionType where
  dquoteTxt = \case
    ObjectField    -> "Object"
    InterfaceField -> "Interface"
    EnumField      -> "Enum"

data ArgumentDefinitionType
  = InputObjectArgument
  | DirectiveArgument
  deriving (Show, Eq)

instance DQuote ArgumentDefinitionType where
  dquoteTxt = \case
    InputObjectArgument -> "Input object"
    DirectiveArgument   -> "Directive"

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

instance DQuote GraphQLType where
  dquoteTxt = \case
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
  | FieldDoesNotExist !GraphQLType !G.Name
  -- ^ error to indicate when a field doesn't exist
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
  | NonExistingScalar !G.Name
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
  | MultipleArgumentsInPresetFound
  | ExpectedListButGotNameType
  deriving (Show, Eq)

showRoleBasedSchemaValidationError :: RoleBasedSchemaValidationError -> Text
showRoleBasedSchemaValidationError = \case
  NonMatchingType fldName fldType expectedType providedType ->
    "expected type of " <> fldName <<> "(" <> fldType <<> ")" <>" to be " <>
    (G.showGT expectedType) <> " but recieved " <> (G.showGT providedType)
  FieldDoesNotExist fldType fldName ->
    fldType <<> ": " <> fldName <<> " does not exist in the upstream remote schema"
  NonMatchingDefaultValue inpObjName inpValName expectedVal providedVal ->
    "expected default value of input value: " <> inpValName <<> "of input object "
    <> inpObjName <<> " to be "
    -- TODO: DO NOT use "show" below, write a DQuote instance for Value Void
    <> (T.pack $ show expectedVal) <> " but recieved " <> (T.pack $ show providedVal)
  NonExistingInputArgument inpObjName inpArgName ->
    "input argument " <> inpArgName <<> " does not exist in the input object:" <>> inpObjName
  MissingNonNullableArguments fieldName nonNullableArgs ->
    "field: " <> fieldName <<> " expects the following non nullable arguments to "
    <> "be present: " <> (englishList "and" $ fmap dquoteTxt nonNullableArgs)
  NonExistingDirectiveArgument parentName parentType directiveName nonExistingArgs ->
    "the following directive argument(s) defined in the directive: "
    <> directiveName
    <<> " defined with the type name: "
    <> parentName <<> " of type "
    <> parentType <<> " do not exist in the corresponding upstream directive: "
    <> (englishList "and" $ fmap dquoteTxt nonExistingArgs)
  NonExistingField (fldDefnType, parentTypeName) providedName ->
    "field " <> providedName <<> " does not exist in the "
    <> fldDefnType <<> ": " <>> parentTypeName
  NonExistingScalar scalarName ->
    "scalar " <> scalarName <<> " does not exist in the upstream remote schema"
  NonExistingUnionMemberTypes unionName nonExistingMembers ->
    "union " <> unionName <<> " contains members which do not exist in the members"
    <> " of the remote schema union :"
    <> (englishList "and" $ fmap dquoteTxt nonExistingMembers)
  CustomInterfacesNotAllowed objName customInterfaces ->
    "custom interfaces are not supported. " <> "Object" <> objName
    <<> " implements the following custom interfaces: "
    <> (englishList "and" $ fmap dquoteTxt customInterfaces)
  ObjectImplementsNonExistingInterfaces objName nonExistentInterfaces ->
    "object " <> objName <<> " is trying to implement the following interfaces"
    <> " that do not exist in the corresponding upstream remote object: "
    <> (englishList "and" $ fmap dquoteTxt nonExistentInterfaces)
  NonExistingEnumValues enumName nonExistentEnumVals ->
    "enum " <> enumName <<> " contains the following enum values that do not exist "
    <> "in the corresponding upstream remote enum: " <>
    (englishList "and" $ fmap dquoteTxt nonExistentEnumVals)
  MissingQueryRoot -> "query root does not exist in the schema definition"
  MultipleSchemaDefinitionsFound -> "multiple schema definitions found"
  DuplicateTypeNames typeNames ->
    "duplicate type names found: "
    <> (englishList "and" $ fmap dquoteTxt typeNames)
  DuplicateDirectives (parentType, parentName) directiveNames ->
    "duplicate directives: " <> (englishList "and" $ fmap dquoteTxt directiveNames)
    <> "found in the " <> parentType <<> " " <>> parentName
  DuplicateFields (parentType, parentName) fieldNames ->
    "duplicate fields: " <> (englishList "and" $ fmap dquoteTxt fieldNames)
    <> "found in the " <> parentType <<> " " <>> parentName
  DuplicateArguments fieldName args ->
    "duplicate arguments: "
    <> (englishList "and" $ fmap dquoteTxt args)
    <> "found in the field: " <>> fieldName
  DuplicateEnumValues enumName enumValues ->
    "duplicate enum values: " <> (englishList "and" $ fmap dquoteTxt enumValues)
    <> " found in the " <> enumName <<> " enum"
  InvalidPresetDirectiveLocation ->
    "Preset directives can be defined only on INPUT_FIELD_DEFINITION or ARGUMENT_DEFINITION"
  MultiplePresetDirectives (parentType, parentName) ->
    "found multiple preset directives at " <> parentType <<> " " <>> parentName
  NoPresetArgumentFound -> "no arguments found in the preset directive"
  InvalidPresetArgument argName ->
    "expected preset argument \"value\" but found " <>> argName
  MultipleArgumentsInPresetFound -> "expected only one preset argument \"value\" but found multiple preset arguments"

presetValueScalar :: G.ScalarTypeDefinition
presetValueScalar = G.ScalarTypeDefinition Nothing $$(G.litName "PresetValue") mempty

presetDirectiveDefn :: G.DirectiveDefinition G.InputValueDefinition
presetDirectiveDefn =
  G.DirectiveDefinition Nothing $$(G.litName "preset") [directiveArg] directiveLocations
  where
    gType = G.TypeNamed (G.Nullability False) $ $$(G.litName "PresetValue")

    directiveLocations = map G.DLTypeSystem [G.TSDLARGUMENT_DEFINITION, G.TSDLINPUT_FIELD_DEFINITION]

    directiveArg = G.InputValueDefinition Nothing $$(G.litName "value") gType Nothing mempty

presetDirectiveName :: G.Name
presetDirectiveName = $$(G.litName "preset")

parsePresetDirective
  :: forall m
  .  (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.GType
  -> G.Directive Void
  -> m [RemoteSchemaPresetArgument]
parsePresetDirective gType (G.Directive name args) = do
  case (Map.toList args) of
    [] -> refute $ pure $ NoPresetArgumentFound
    [(argName, argVal)] -> do
      -- TODO: typecheck `argVal` with the GType, along with which it was provided?
      -- I guess this will only be possible only in the case of static presets
      unless (argName == $$(G.litName "value")) $ do
        refute $ pure $ InvalidPresetArgument argName
      mkPresetArgument gType argVal
    _ -> refute $ pure $ MultipleArgumentsInPresetFound
  where
    mkPresetArgument :: G.GType -> G.Value Void -> m [RemoteSchemaPresetArgument]
    mkPresetArgument gType' = \case
      G.VString t ->
        case isSessionVariable t of
          True  -> pure . pure $ (SessionPresetArgument gType' $ mkSessionVariable t)
          False -> pure . pure $ StaticPresetArgument $ G.VString t
      G.VObject obj ->
        case gType' of
          G.TypeNamed _ _ -> do
            unless (G.isListType gType') $ do
              refute $ pure $ ExpectedListButGotNameType
            pure [] -- TODO: is this correct?
          G.TypeList _ gType'' ->
            concat . Map.elems <$> traverse (mkPresetArgument gType'') obj
      v -> pure . pure $ StaticPresetArgument v

-- | validateDirective checks if the arguments of a given directive
--   are a subset of the corresponding upstream directive arguments
validateDirective
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.Directive a -- ^ provided directive
  -> G.Directive a -- ^ upstream directive
  -> (GraphQLType, G.Name) -- ^ parent type and name
  -> m ()
validateDirective providedDirective upstreamDirective (parentType, parentTypeName) = do
  onJust (NE.nonEmpty $ Map.keys argsDiff) $ \argNames ->
    dispute $ pure $
      NonExistingDirectiveArgument parentTypeName parentType directiveName argNames
  where
    argsDiff = Map.difference providedDirectiveArgs upstreamDirectiveArgs

    providedDirectiveArgs = G._dArguments providedDirective
    upstreamDirectiveArgs = G._dArguments upstreamDirective

    directiveName = G._dName providedDirective

-- | validateDirectives checks if the `providedDirectives`
-- are a subset of `upstreamDirectives` and then validate
-- each of the directives by calling the `validateDirective`
validateDirectives
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.Directive a]
  -> [G.Directive a]
  -> G.TypeSystemDirectiveLocation
  -> (GraphQLType, G.Name)
  -> m (Maybe (G.Directive a))
validateDirectives providedDirectives upstreamDirectives directiveLocation parentType = do
  onJust (NE.nonEmpty $ duplicates $ map G._dName nonPresetDirectives) $ \dups -> do
    refute $ pure $ DuplicateDirectives parentType dups
  flip traverse_ nonPresetDirectives $ \dir -> do
    let directiveName = G._dName dir
    upstreamDir <-
      onNothing (Map.lookup directiveName upstreamDirectivesMap) $
        refute $ pure $ FieldDoesNotExist Directive directiveName
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

getDifference :: (Eq a, Hashable a) => [a] -> [a] -> S.HashSet a
getDifference left right = S.difference (S.fromList left) (S.fromList right)

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
  -> m ()
validateEnumTypeDefinition providedEnum upstreamEnum = do
  validateDirectives providedDirectives upstreamDirectives G.TSDLENUM $ (Enum, providedName)
  onJust (NE.nonEmpty $ duplicates providedEnumValNames) $ \dups -> do
    refute $ pure $ DuplicateEnumValues providedName dups
  onJust (NE.nonEmpty $ S.toList fieldsDifference) $ \nonExistingEnumVals ->
    dispute $ pure $ NonExistingEnumValues providedName nonExistingEnumVals
  where
    G.EnumTypeDefinition _ providedName providedDirectives providedValueDefns = providedEnum

    G.EnumTypeDefinition _ _ upstreamDirectives upstreamValueDefns = upstreamEnum

    providedEnumValNames   = map (G.unEnumValue . G._evdName) $ providedValueDefns

    upstreamEnumValNames   = map (G.unEnumValue . G._evdName) $ upstreamValueDefns

    fieldsDifference       = getDifference providedEnumValNames upstreamEnumValNames

-- | `validateEnumTypeDefinitions` checks if the `providedEnums`
-- is a subset of `upstreamEnums`. Then, each enum provided by
-- the user is validated against the corresponding upstream enum
-- by calling the `validateEnumTypeDefinition`
validateEnumTypeDefinitions
  :: ( MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.EnumTypeDefinition]
  -> [G.EnumTypeDefinition]
  -> m ()
validateEnumTypeDefinitions providedEnums upstreamEnums = do
  flip traverse_ providedEnums $ \providedEnum@(G.EnumTypeDefinition _ name _ _) -> do
    upstreamEnum <-
      onNothing (Map.lookup name upstreamEnumsMap) $
        refute $ pure $ FieldDoesNotExist Enum name
    validateEnumTypeDefinition providedEnum upstreamEnum
  where
    upstreamEnumsMap = mapFromL G._etdName $ upstreamEnums

validateInputValueDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.InputValueDefinition
  -> G.InputValueDefinition
  -> G.Name
  -> m RemoteSchemaInputValueDefinition
validateInputValueDefinition providedDefn upstreamDefn inputObjectName = do
  presetDirective <-
    validateDirectives providedDirectives upstreamDirectives G.TSDLINPUT_FIELD_DEFINITION
     $ (Argument InputObjectArgument, inputObjectName)
  when (providedType /= upstreamType) $
    dispute $ pure $
      NonMatchingType providedName (Argument InputObjectArgument) providedType upstreamType
  when (providedDefaultValue /= upstreamDefaultValue) $
    dispute $ pure $
      NonMatchingDefaultValue inputObjectName providedName
                              upstreamDefaultValue providedDefaultValue
  presetArguments <- for presetDirective $ parsePresetDirective providedType
  pure $ RemoteSchemaInputValueDefinition providedDefn presetArguments
  where
    G.InputValueDefinition _ providedName providedType providedDefaultValue providedDirectives  = providedDefn
    G.InputValueDefinition _ _ upstreamType upstreamDefaultValue upstreamDirectives  = upstreamDefn

validateArguments
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => (G.ArgumentsDefinition G.InputValueDefinition)
  -> (G.ArgumentsDefinition RemoteSchemaInputValueDefinition)
  -> G.Name
  -> m [RemoteSchemaInputValueDefinition]
validateArguments providedArgs upstreamArgs parentTypeName = do
  onJust (NE.nonEmpty $ duplicates $ map G._ivdName providedArgs) $ \dups -> do
    refute $ pure $ DuplicateArguments parentTypeName dups
  let argsDiff = getDifference nonNullableUpstreamArgs nonNullableProvidedArgs
  onJust (NE.nonEmpty $ S.toList argsDiff) $ \nonNullableArgs -> do
    refute $ pure $ MissingNonNullableArguments parentTypeName nonNullableArgs
  flip traverse providedArgs $ \providedArg@(G.InputValueDefinition _ name _ _ _) -> do
    upstreamArg <-
      onNothing (Map.lookup name upstreamArgsMap) $
        refute $ pure $ NonExistingInputArgument parentTypeName name
    validateInputValueDefinition providedArg upstreamArg parentTypeName
  where
    upstreamArgsMap = mapFromL G._ivdName $  map _rsitdDefn upstreamArgs

    nonNullableUpstreamArgs = map G._ivdName $ filter (not . G.isNullable . G._ivdType) $ map _rsitdDefn upstreamArgs

    nonNullableProvidedArgs = map G._ivdName $ filter (not . G.isNullable . G._ivdType) providedArgs

validateInputObjectTypeDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.InputObjectTypeDefinition G.InputValueDefinition
  -> G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition
  -> m (G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition)
validateInputObjectTypeDefinition providedInputObj upstreamInputObj = do
  validateDirectives providedDirectives upstreamDirectives G.TSDLINPUT_OBJECT $ (InputObject, providedName)
  args <- validateArguments providedArgs upstreamArgs $ providedName
  pure $ providedInputObj { G._iotdValueDefinitions = args }
  where
    G.InputObjectTypeDefinition _ providedName providedDirectives providedArgs = providedInputObj

    G.InputObjectTypeDefinition _ _ upstreamDirectives upstreamArgs = upstreamInputObj

validateInputObjectTypeDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.InputObjectTypeDefinition G.InputValueDefinition]
  -> [G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition]
  -> m [G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition]
validateInputObjectTypeDefinitions providedInputObjects upstreamInputObjects = do
  flip traverse providedInputObjects $ \providedInputObject@(G.InputObjectTypeDefinition _ name _ _) -> do
    upstreamInputObject <-
      onNothing (Map.lookup name upstreamInputObjectsMap) $
        refute $ pure $ FieldDoesNotExist InputObject name
    validateInputObjectTypeDefinition providedInputObject upstreamInputObject
  where
    upstreamInputObjectsMap = mapFromL G._iotdName $ upstreamInputObjects

validateFieldDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => (G.FieldDefinition G.InputValueDefinition)
  -> (G.FieldDefinition RemoteSchemaInputValueDefinition)
  -> (FieldDefinitionType, G.Name)
  -> m (G.FieldDefinition RemoteSchemaInputValueDefinition)
validateFieldDefinition providedFieldDefinition upstreamFieldDefinition (parentType, parentTypeName) = do
  validateDirectives providedDirectives upstreamDirectives G.TSDLFIELD_DEFINITION $ (Field parentType, parentTypeName)
  when (providedType /= upstreamType) $
    dispute $ pure $ NonMatchingType providedName (Field parentType) upstreamType providedType
  args <- validateArguments providedArgs upstreamArgs $ providedName
  pure $ providedFieldDefinition { G._fldArgumentsDefinition = args }
  where
    G.FieldDefinition _ providedName providedArgs providedType providedDirectives = providedFieldDefinition

    G.FieldDefinition _ _ upstreamArgs upstreamType upstreamDirectives = upstreamFieldDefinition

validateFieldDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [(G.FieldDefinition G.InputValueDefinition)]
  -> [(G.FieldDefinition RemoteSchemaInputValueDefinition)]
  -> (FieldDefinitionType, G.Name) -- ^ parent type and name
  -> m [(G.FieldDefinition RemoteSchemaInputValueDefinition)]
validateFieldDefinitions providedFldDefnitions upstreamFldDefinitions parentType = do
  onJust (NE.nonEmpty $ duplicates $ map G._fldName providedFldDefnitions) $ \dups -> do
    refute $ pure $ DuplicateFields parentType dups
  flip traverse providedFldDefnitions $ \fldDefn@(G.FieldDefinition _ name _ _ _) -> do
    upstreamFldDefn <-
      onNothing (Map.lookup name upstreamFldDefinitionsMap) $
        refute $ pure $ NonExistingField parentType name
    validateFieldDefinition fldDefn upstreamFldDefn parentType
  where
    upstreamFldDefinitionsMap = mapFromL G._fldName upstreamFldDefinitions

validateInterfaceDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.InterfaceTypeDefinition G.InputValueDefinition ()
  -> G.InterfaceTypeDefinition RemoteSchemaInputValueDefinition ()
  -> m (G.InterfaceTypeDefinition RemoteSchemaInputValueDefinition ())
validateInterfaceDefinition providedInterfaceDefn upstreamInterfaceDefn = do
  validateDirectives providedDirectives upstreamDirectives G.TSDLINTERFACE $ (Interface, providedName)
  fieldDefinitions <- validateFieldDefinitions providedFieldDefns upstreamFieldDefns $ (InterfaceField, providedName)
  pure $ providedInterfaceDefn { G._itdFieldsDefinition = fieldDefinitions }
  where
    G.InterfaceTypeDefinition _ providedName providedDirectives providedFieldDefns _ = providedInterfaceDefn

    G.InterfaceTypeDefinition _ _ upstreamDirectives upstreamFieldDefns _ = upstreamInterfaceDefn

validateInterfaceDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.InterfaceTypeDefinition G.InputValueDefinition ()]
  -> [G.InterfaceTypeDefinition RemoteSchemaInputValueDefinition ()]
  -> m [(G.InterfaceTypeDefinition RemoteSchemaInputValueDefinition ())]
validateInterfaceDefinitions providedInterfaces upstreamInterfaces = do
  flip traverse providedInterfaces $ \providedInterface@(G.InterfaceTypeDefinition _ name _ _ _) -> do
    upstreamInterface <-
      onNothing (Map.lookup name upstreamInterfacesMap) $
        refute $ pure $ FieldDoesNotExist Interface name
    validateInterfaceDefinition providedInterface upstreamInterface
  where
    upstreamInterfacesMap = mapFromL G._itdName $ upstreamInterfaces

validateScalarDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.ScalarTypeDefinition
  -> G.ScalarTypeDefinition
  -> m ()
validateScalarDefinition providedScalar upstreamScalar = do
  void $ validateDirectives providedDirectives upstreamDirectives G.TSDLSCALAR $ (Scalar, providedName)
  where
    G.ScalarTypeDefinition _ providedName providedDirectives = providedScalar

    G.ScalarTypeDefinition _ _ upstreamDirectives = upstreamScalar

validateScalarDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.ScalarTypeDefinition]
  -> [G.ScalarTypeDefinition]
  -> m ()
validateScalarDefinitions providedScalars upstreamScalars = do
  flip traverse_ providedScalars $ \providedScalar@(G.ScalarTypeDefinition _ name _) -> do
    upstreamScalar <-
      onNothing (Map.lookup name upstreamScalarsMap) $
        refute $ pure $ NonExistingScalar name
    validateScalarDefinition providedScalar upstreamScalar
  where
    upstreamScalarsMap = mapFromL G._stdName upstreamScalars

validateUnionDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => G.UnionTypeDefinition
  -> G.UnionTypeDefinition
  -> m ()
validateUnionDefinition providedUnion upstreamUnion = do
  validateDirectives providedDirectives upstreamDirectives G.TSDLUNION $ (Union, providedName)
  onJust (NE.nonEmpty $ S.toList memberTypesDiff) $ \nonExistingMembers ->
    refute $ pure $ NonExistingUnionMemberTypes providedName nonExistingMembers
  where
    G.UnionTypeDefinition _ providedName providedDirectives providedMemberTypes = providedUnion

    G.UnionTypeDefinition _ _ upstreamDirectives upstreamMemberTypes = upstreamUnion

    memberTypesDiff = getDifference providedMemberTypes upstreamMemberTypes

validateUnionTypeDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.UnionTypeDefinition]
  -> [G.UnionTypeDefinition]
  -> m ()
validateUnionTypeDefinitions providedUnions upstreamUnions = do
  flip traverse_ providedUnions $ \providedUnion@(G.UnionTypeDefinition _ name _ _) -> do
    upstreamUnion <-
      onNothing (Map.lookup name upstreamUnionsMap) $
        refute $ pure $ FieldDoesNotExist Union name
    validateUnionDefinition providedUnion upstreamUnion
  where
    upstreamUnionsMap = mapFromL G._utdName $ upstreamUnions

validateObjectDefinition
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => (G.ObjectTypeDefinition G.InputValueDefinition)
  -> (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
  -> S.HashSet G.Name -- ^ Interfaces declared by in the role-based schema
  -> m (G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)
validateObjectDefinition providedObj upstreamObj interfacesDeclared = do
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

    G.ObjectTypeDefinition _ _
       upstreamIfaces upstreamDirectives upstreamFldDefnitions = upstreamObj

    interfacesDiff = getDifference providedIfaces upstreamIfaces

    providedIfacesSet = S.fromList providedIfaces

    customInterfaces = S.intersection interfacesDiff interfacesDeclared

    nonExistingInterfaces = S.toList $ S.difference interfacesDiff providedIfacesSet

-- | function to compare objects of the role based schema against the
-- objects of the upstream remote schema.
validateObjectDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.ObjectTypeDefinition G.InputValueDefinition]
  -> [G.ObjectTypeDefinition RemoteSchemaInputValueDefinition]
  -> S.HashSet G.Name
  -> m [G.ObjectTypeDefinition RemoteSchemaInputValueDefinition]
validateObjectDefinitions providedObjects upstreamObjects providedInterfaces = do
  flip traverse providedObjects $ \providedObject@(G.ObjectTypeDefinition _ name _ _ _) -> do
    upstreamObject <-
      onNothing (Map.lookup name upstreamObjectsMap) $
        refute $ pure $ FieldDoesNotExist Object name
    validateObjectDefinition providedObject upstreamObject providedInterfaces
  where
    upstreamObjectsMap = mapFromL G._otdName $ upstreamObjects

-- | helper function to validate the schema definitions mentioned in the schema
-- document.
validateSchemaDefinitions
  :: (MonadValidate [RoleBasedSchemaValidationError] m)
  => [G.SchemaDefinition]
  -> m (G.Name, Maybe G.Name, Maybe G.Name)
validateSchemaDefinitions [] =
  -- The spec says that if a Schema document's root operation names
  -- for the query root, mutation root and the subscription root are
  -- "Query", "Mutation" and "Subscription", then it can be omitted
  -- from the schema document.
  -- https://spec.graphql.org/June2018/#sec-Root-Operation-Types
  pure $ ($$(G.litName "Query"), Just $$(G.litName "Mutation"), Just $$(G.litName "Subscription"))
validateSchemaDefinitions [schemaDefn] = do
  let G.SchemaDefinition _ rootOpsTypes = schemaDefn
      rootOpsTypesMap = mapFromL G._rotdOperationType rootOpsTypes
      mQueryRootName = G._rotdOperationTypeType <$> Map.lookup G.OperationTypeQuery rootOpsTypesMap
      mMutationRootName = G._rotdOperationTypeType <$> Map.lookup G.OperationTypeMutation rootOpsTypesMap
      mSubscriptionRootName = G._rotdOperationTypeType <$> Map.lookup G.OperationTypeSubscription rootOpsTypesMap
  queryRootName <-
    onNothing mQueryRootName $
      refute $ pure $ MissingQueryRoot
  pure (queryRootName, mMutationRootName, mSubscriptionRootName)
validateSchemaDefinitions _ = refute $ pure $ MultipleSchemaDefinitionsFound

-- | Construction of the `possibleTypes` map for interfaces, while parsing the
-- user provided Schema document, it doesn't include the `possibleTypes`, so
-- constructing here, manually.
createPossibleTypesMap :: [(G.ObjectTypeDefinition RemoteSchemaInputValueDefinition)] -> HashMap G.Name [G.Name]
createPossibleTypesMap objDefns =
  let objMap = Map.fromList $ map (G._otdName &&& G._otdImplementsInterfaces) objDefns
  in
  Map.foldlWithKey' (\acc objTypeName interfaces ->
                       let interfaceMap =
                             Map.fromList $ map (\iface -> (iface, [objTypeName])) interfaces
                       in
                       Map.unionWith (<>) acc interfaceMap)
                    mempty
                    objMap

-- | getSchemaDocIntrospection converts the `PartitionedTypeDefinitions` to
-- `IntrospectionResult` because the function `buildRemoteParser` function which
-- builds the remote schema parsers accepts an `IntrospectionResult`. The
-- conversion involves converting `G.TypeDefinition ()` to `G.TypeDefinition
-- [G.Name]`. The `[G.Name]` here being the list of object names that an
-- interface implements. This is needed to be done here by-hand because while
-- specifying the `SchemaDocument` through the GraphQL DSL, it doesn't include
-- the `possibleTypes` along with an object.
getSchemaDocIntrospection
  :: [G.ScalarTypeDefinition]
  -> [G.EnumTypeDefinition]
  -> [G.InterfaceTypeDefinition RemoteSchemaInputValueDefinition ()]
  -> [G.UnionTypeDefinition]
  -> [G.InputObjectTypeDefinition RemoteSchemaInputValueDefinition]
  -> [G.ObjectTypeDefinition RemoteSchemaInputValueDefinition]
  -> (G.Name, Maybe G.Name, Maybe G.Name)
  -> IntrospectionResult
getSchemaDocIntrospection scalars enums interfaces unions inpObjs objects (queryRoot, mutationRoot, subscriptionRoot) =
  let scalarTypeDefs = map G.TypeDefinitionScalar $ scalars <> defaultScalars
      objectTypeDefs = map G.TypeDefinitionObject objects
      unionTypeDefs = map G.TypeDefinitionUnion unions
      enumTypeDefs = map G.TypeDefinitionEnum enums
      inpObjTypeDefs = map G.TypeDefinitionInputObject inpObjs

      possibleTypesMap = createPossibleTypesMap objects
      interfacesWithPossibleTypes = map (\iface ->
                                           let name = G._itdName iface
                                           in
                                             iface
                                               {G._itdPossibleTypes =
                                                fromMaybe [] (Map.lookup name possibleTypesMap)})
                                           interfaces
      interfaceTypeDefs = map G.TypeDefinitionInterface $ interfacesWithPossibleTypes
      modifiedTypeDefs =
        scalarTypeDefs <> objectTypeDefs
        <> interfaceTypeDefs <> unionTypeDefs
        <> enumTypeDefs <> inpObjTypeDefs
      schemaIntrospection = RemoteSchemaIntrospection modifiedTypeDefs
  in IntrospectionResult schemaIntrospection queryRoot mutationRoot subscriptionRoot
  where
    defaultScalars = map (\n -> G.ScalarTypeDefinition Nothing n [])
                         $ [intScalar, floatScalar, stringScalar, boolScalar, idScalar]

partitionTypeDefinition :: G.TypeDefinition a () -> State (PartitionedTypeDefinitions a) ()
partitionTypeDefinition (G.TypeDefinitionScalar scalarDefn) =
  modify (\td -> td {_ptdScalars = ((:) scalarDefn) . _ptdScalars $ td})
partitionTypeDefinition (G.TypeDefinitionObject objectDefn) =
  modify (\td -> td {_ptdObjects = ((:) objectDefn) . _ptdObjects $ td})
partitionTypeDefinition (G.TypeDefinitionInterface interfaceDefn) =
  modify (\td -> td {_ptdInterfaces = ((:) interfaceDefn) . _ptdInterfaces $ td})
partitionTypeDefinition (G.TypeDefinitionUnion unionDefn) =
  modify (\td -> td {_ptdUnions = ((:) unionDefn) . _ptdUnions $ td})
partitionTypeDefinition (G.TypeDefinitionEnum enumDefn) =
  modify (\td -> td {_ptdEnums = ((:) enumDefn) . _ptdEnums $ td})
partitionTypeDefinition (G.TypeDefinitionInputObject inputObjectDefn) =
  modify (\td -> td {_ptdInputObjects = ((:) inputObjectDefn) . _ptdInputObjects $ td})

-- | validateRemoteSchema accepts two arguments, the `SchemaDocument` of
-- the role-based schema, that is provided by the user and the `SchemaIntrospection`
-- of the upstream remote schema. This function, in turn calls the other validation
-- functions for scalars, enums, unions, interfaces,input objects and objects.
validateRemoteSchema
  :: ( MonadValidate [RoleBasedSchemaValidationError] m)
  => G.SchemaDocument
  -> RemoteSchemaIntrospection
  -> m IntrospectionResult
validateRemoteSchema (G.SchemaDocument providedTypeDefns) (RemoteSchemaIntrospection upstreamTypeDefns) = do
  let
    -- Converting `[G.TypeSystemDefinition]` into `PartitionedTypeDefinitions`
    (_, providedTypes) = flip runState emptySchemaDocTypeDefinitions $
                      traverse partitionTypeSystemDefinitions providedTypeDefns
    -- Converting `[G.TypeDefinition [Name]]` into `PartitionedTypeDefinitions`
    (_, upstreamTypes) = flip runState emptySchemaDocTypeDefinitions $
                      traverse partitionSchemaIntrospection upstreamTypeDefns
    providedInterfacesList = map G._itdName $ _ptdInterfaces providedTypes
    duplicateTypesList = duplicateTypes providedTypes
  -- check for duplicate type names
  onJust (NE.nonEmpty duplicateTypesList) $ \duplicateTypeNames ->
    refute $ pure $ DuplicateTypeNames duplicateTypeNames
  rootTypeNames <- validateSchemaDefinitions $ _ptdSchemaDef providedTypes
  validateScalarDefinitions (_ptdScalars providedTypes) (_ptdScalars upstreamTypes)
  validateEnumTypeDefinitions (_ptdEnums providedTypes) (_ptdEnums upstreamTypes)
  interfaceTypeDefns <- validateInterfaceDefinitions (_ptdInterfaces providedTypes) (_ptdInterfaces upstreamTypes)
  validateUnionTypeDefinitions (_ptdUnions providedTypes) (_ptdUnions upstreamTypes)
  inpObjTypeDefns <- validateInputObjectTypeDefinitions (_ptdInputObjects providedTypes) (_ptdInputObjects upstreamTypes)
  objTypeDefns <- validateObjectDefinitions (_ptdObjects providedTypes) (_ptdObjects upstreamTypes) $ S.fromList providedInterfacesList
  let PartitionedTypeDefinitions scalars _ _ unions enums _ _ = providedTypes
  pure $ getSchemaDocIntrospection scalars enums interfaceTypeDefns unions inpObjTypeDefns objTypeDefns rootTypeNames
  where
    emptySchemaDocTypeDefinitions = PartitionedTypeDefinitions [] [] [] [] [] [] []

    partitionTypeSystemDefinitions :: G.TypeSystemDefinition -> State (PartitionedTypeDefinitions G.InputValueDefinition) ()
    partitionTypeSystemDefinitions (G.TypeSystemDefinitionSchema schemaDefn) =
      modify (\td -> td {_ptdSchemaDef = ((:) schemaDefn) . _ptdSchemaDef $ td})
    partitionTypeSystemDefinitions (G.TypeSystemDefinitionType typeDefn) =
      partitionTypeDefinition typeDefn

    partitionSchemaIntrospection :: G.TypeDefinition a [G.Name] -> State (PartitionedTypeDefinitions a) ()
    partitionSchemaIntrospection typeDef = partitionTypeDefinition (typeDef $> ())

    duplicateTypes (PartitionedTypeDefinitions scalars objs ifaces unions enums inpObjs _) =
      duplicates $
      (map G._stdName scalars) <> (map G._otdName objs) <> (map G._itdName ifaces)
      <> (map G._utdName unions) <> (map G._etdName enums) <> (map G._iotdName inpObjs)

resolveRoleBasedRemoteSchema
  :: (MonadError QErr m)
  => G.SchemaDocument
  -> RemoteSchemaCtx
  -> m (IntrospectionResult, [SchemaDependency])
resolveRoleBasedRemoteSchema providedSchemaDoc upstreamRemoteCtx = do
  introspectionRes <-
    either (throw400 InvalidRoleBasedRemoteSchema . showErrors) pure
    =<< runValidateT (validateRemoteSchema providedSchemaDoc $ irDoc $ rscIntro upstreamRemoteCtx)
  pure (introspectionRes, [schemaDependency])
  where
    showErrors :: [RoleBasedSchemaValidationError] -> Text
    showErrors errors =
      "validation for the given role-based schema failed " <> reasonsMessage
      where
        reasonsMessage = case errors of
          [] -> "" -- this case is impossible
          [singleError] -> "because " <> showRoleBasedSchemaValidationError singleError
          _ -> "for the following reasons:\n" <> T.unlines
             (map ((" • " <>) . showRoleBasedSchemaValidationError) errors)

    schemaDependency = SchemaDependency (SORemoteSchema $ rscName upstreamRemoteCtx) DRRemoteSchema
