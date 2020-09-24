module Hasura.RQL.DDL.RemoteSchema.Validate () where

import           Control.Monad.Validate

import           Hasura.Prelude
import           Hasura.RQL.Types              hiding (GraphQLType)
import           Hasura.SQL.Types
import           Hasura.Server.Utils           (englishList)

import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Data.HashSet                  as S
import qualified Data.List.NonEmpty            as NE


data SchemaDocumentTypeDefinitions
  = SchemaDocumentTypeDefinitions
  { _sdtdScalars      :: ![G.ScalarTypeDefinition]
  , _sdtdObjects      :: ![G.ObjectTypeDefinition]
  , _sdtdInterfaces   :: ![G.InterfaceTypeDefinition ()]
  , _sdtdUnions       :: ![G.UnionTypeDefinition]
  , _sdtdEnums        :: ![G.EnumTypeDefinition]
  , _sdtdInputObjects :: ![G.InputObjectTypeDefinition]
  , _sdtdSchemaDef    :: ![G.SchemaDefinition]
  } deriving (Show, Eq)

data FieldDefinitionType = ObjectField | InterfaceField deriving (Show, Eq)

data ArgumentDefinitionType = InputObjectArgument | DirectiveArgument deriving (Show, Eq)

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

data CustomRemoteSchemaValidationError
  = NonMatchingType !G.Name !GraphQLType !G.GType !G.GType
  -- gType - expected name - provided name
  -- ^ error to indicate that an user provided
  -- type differs from the type defined in the upstream
  -- remote schema
  | ExtraneousFields !GraphQLType !G.Name !(S.HashSet G.Name)
  -- ^ error to indicate when an user provided type
  -- has more fields than what's defined
  -- in the remote schema
  | ExtraneousArgs !GraphQLType !G.Name !(NE.NonEmpty G.Name)
  -- ^ error to indicate when an user provided type
  -- has more arguments than what's defined in a particular
  -- type in the remote schema
  | DifferingArgs !GraphQLType !(NE.NonEmpty G.Name)
  | FieldDoesNotExist !GraphQLType !G.Name
  -- ^ error to indicate when a single field doesn't exist
  -- in the upstream remote schema
  | NonMatchingDefaultValue !G.Name !G.Name !(Maybe (G.Value Void)) !(Maybe (G.Value Void))
  -- ^ input-object-name - input-value-name - expected-def-val - provided-def-val
  | NonExistingInputArgument !G.Name !G.Name
  -- ^ input-object-name - argument-name
  | NonExistingDirectiveArgument !G.Name ![G.Name]
  -- ^ directive-name - argument-name
  | NonExistingField !G.Name !FieldDefinitionType !G.Name
  -- ^ parent-type-name - "object/interface" - provided-name
  | NonExistingScalar !G.Name
  | NonExistingUnionMemberTypes !G.Name !(S.HashSet G.Name)
  -- ^ union name - extra-fields
  | CustomInterfacesNotAllowed !G.Name !(NE.NonEmpty G.Name)
  -- ^ object name - custom interfaces
  | ObjectImplementsNonExistingInterfaces !G.Name !(NE.NonEmpty G.Name)
  -- ^ object-name
  deriving (Show, Eq)

validateDirective
  :: (Eq a, MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.Directive a -- ^ provided directive definition
  -> G.Directive a -- ^ original directive definition
  -> (GraphQLType, G.Name) -- ^ parent type and name
  -> m ()
validateDirective providedDirective originalDirective gType = do
  if argsDiff == Map.empty
    then pure ()
    else
    dispute $ pure $ NonExistingDirectiveArgument directiveName $ Map.keys argsDiff
  where
    argsDiff = Map.difference providedDirectiveArgs originalDirectiveArgs

    providedDirectiveArgs = G._dArguments providedDirective
    originalDirectiveArgs = G._dArguments originalDirective

    extraneousKeys = Map.keys $ Map.difference providedDirectiveArgs originalDirectiveArgs
    directiveName = G._dName providedDirective

validateDirectives
  :: ( Eq a
     , MonadValidate [CustomRemoteSchemaValidationError] m
     )
  => [G.Directive a]
  -> [G.Directive a]
  -> (GraphQLType, G.Name)
  -> m ()
validateDirectives providedDirectives originalDirectives gType =
  flip traverse_ providedDirectives $ \dir -> do
    let directiveName = G._dName dir
    originalDir <-
      onNothing (Map.lookup directiveName originalDirectivesMap) $
        refute $ pure $ FieldDoesNotExist Directive directiveName
    validateDirective dir originalDir gType
  where
    originalDirectivesMap = mapFromL G._dName originalDirectives

getDifference :: (Eq a, Hashable a) => [a] -> [a] -> S.HashSet a
getDifference left right = S.difference (S.fromList left) (S.fromList right)

formatErrorMsg :: S.HashSet G.Name -> Text
formatErrorMsg errorItems =
  let errorItemsList = S.toList errorItems
  in
    case NE.nonEmpty errorItemsList of
      Nothing     -> ""
      Just neList -> englishList "and" $ fmap dquoteTxt neList

-- helper function to check the validity of an enum definition
-- provided by the user against the enum definition defined
-- in the remote
validateEnumTypeDefinition
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.EnumTypeDefinition -- ^ provided enum type definition
  -> G.EnumTypeDefinition -- ^ original enum type definition
  -> m ()
validateEnumTypeDefinition providedDefn originalDefn = do
  validateDirectives (G._etdDirectives providedDefn) (G._etdDirectives originalDefn) $ (Enum, enumFieldName)
  case fieldsDifference of
    empty       -> pure ()
    extraFields -> dispute $ pure $ ExtraneousFields Enum enumFieldName extraFields
  pure ()
  where
    providedEnumDirectives = (G._etdDirectives providedDefn)

    originalEnumDirectives = G._etdDirectives originalDefn

    providedEnumValNames   = map (G.unEnumValue . G._evdName) (G._etdValueDefinitions providedDefn)

    originalEnumValNames   = map (G.unEnumValue . G._evdName) (G._etdValueDefinitions originalDefn)

    fieldsDifference       = getDifference providedEnumValNames originalEnumValNames

    enumFieldName          = G._etdName providedDefn

validateEnumTypeDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
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
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.InputValueDefinition
  -> G.InputValueDefinition
  -> G.Name
  -> m ()
validateInputValueDefinition providedDefn upstreamDefn inputObjectName = do
  when (providedType /= upstreamType) $
    dispute $ pure $ NonMatchingType providedName (Argument InputObjectArgument) providedType upstreamType
  when (providedDefaultValue /= upstreamDefaultValue) $
    dispute $ pure $
      NonMatchingDefaultValue inputObjectName providedName
                              upstreamDefaultValue providedDefaultValue
  pure ()
  where
    G.InputValueDefinition _ providedName providedType providedDefaultValue = providedDefn
    G.InputValueDefinition _ upstreamName upstreamType upstreamDefaultValue = upstreamDefn

validateArguments
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.ArgumentsDefinition
  -> G.ArgumentsDefinition
  -> G.Name
  -> m ()
validateArguments providedArgs upstreamArgs parentTypeName = do
  flip traverse_ providedArgs $ \providedArg@(G.InputValueDefinition _ name _ _) -> do
    upstreamArg <-
      onNothing (Map.lookup name upstreamArgsMap) $
        refute $ pure $ NonExistingInputArgument parentTypeName name
    validateInputValueDefinition providedArg upstreamArg parentTypeName
  where
    upstreamArgsMap = mapFromL G._ivdName $ upstreamArgs

validateInputObjectTypeDefinition
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.InputObjectTypeDefinition
  -> G.InputObjectTypeDefinition
  -> m ()
validateInputObjectTypeDefinition providedInputObj upstreamInputObj = do
  -- 1. Validate the directives
  validateDirectives providedDirectives upstreamDirectives $ (InputObject, providedName)
  validateArguments providedArgs upstreamArgs $ providedName
  where
    G.InputObjectTypeDefinition _ providedName providedDirectives providedArgs = providedInputObj

    G.InputObjectTypeDefinition _ upstreamName upstreamDirectives upstreamArgs = upstreamInputObj

validateInputObjectTypeDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => [G.InputObjectTypeDefinition]
  -> [G.InputObjectTypeDefinition]
  -> m ()
validateInputObjectTypeDefinitions providedInputObjects upstreamInputObjects = do
  flip traverse_ providedInputObjects $ \providedInputObject@(G.InputObjectTypeDefinition _ name _ _) -> do
    upstreamInputObject <-
      onNothing (Map.lookup name upstreamInputObjectsMap) $
        refute $ pure $ FieldDoesNotExist InputObject name
    validateInputObjectTypeDefinition providedInputObject upstreamInputObject
  where
    upstreamInputObjectsMap = mapFromL G._iotdName $ upstreamInputObjects

validateFieldDefinition
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.FieldDefinition
  -> G.FieldDefinition
  -> (FieldDefinitionType, G.Name)
  -> m ()
validateFieldDefinition providedFieldDefinition upstreamFieldDefinition (parentType, parentTypeName) = do
  validateDirectives providedDirectives upstreamDirectives $ (Field parentType, parentTypeName)
  when (providedType /= upstreamType) $
    dispute $ pure $ NonMatchingType providedName (Field parentType) upstreamType providedType
  validateArguments providedArgs upstreamArgs $ providedName
  where
    G.FieldDefinition _ providedName providedArgs providedType providedDirectives = providedFieldDefinition

    G.FieldDefinition _ _ upstreamArgs upstreamType upstreamDirectives = upstreamFieldDefinition

validateFieldDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => [G.FieldDefinition]
  -> [G.FieldDefinition]
  -> (FieldDefinitionType, G.Name) -- ^ parent type and name
  -> m ()
validateFieldDefinitions providedFldDefnitions upstreamFldDefinitions (parentType, parentTypeName) = do
  flip traverse_ providedFldDefnitions $ \fldDefn@(G.FieldDefinition _ name _ _ _) -> do
    upstreamFldDefn <-
      onNothing (Map.lookup name upstreamFldDefinitionsMap) $
        refute $ pure $ NonExistingField parentTypeName parentType name
    validateFieldDefinition fldDefn upstreamFldDefn $ (parentType, parentTypeName)
  where
    upstreamFldDefinitionsMap = mapFromL G._fldName upstreamFldDefinitions

validateInterfaceDefinition
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.InterfaceTypeDefinition ()
  -> G.InterfaceTypeDefinition [G.Name]
  -> m ()
validateInterfaceDefinition providedInterfaceDefn upstreamInterfaceDefn = do
  validateDirectives providedDirectives upstreamDirectives $ (Interface, providedName)
  validateFieldDefinitions providedFieldDefns upstreamFieldDefns $ (InterfaceField, providedName)
  where
    G.InterfaceTypeDefinition _ providedName providedDirectives providedFieldDefns _ = providedInterfaceDefn

    G.InterfaceTypeDefinition _ upstreamName upstreamDirectives upstreamFieldDefns _ = upstreamInterfaceDefn

validateInterfaceDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => [G.InterfaceTypeDefinition ()]
  -> [G.InterfaceTypeDefinition [G.Name]]
  -> m ()
validateInterfaceDefinitions providedInterfaces upstreamInterfaces = do
  flip traverse_ providedInterfaces $ \providedInterface@(G.InterfaceTypeDefinition _ name _ _ _) -> do
    upstreamInterface <-
      onNothing (Map.lookup name upstreamInterfacesMap) $
        refute $ pure $ FieldDoesNotExist Interface name
    validateInterfaceDefinition providedInterface upstreamInterface
  where
    upstreamInterfacesMap = mapFromL G._itdName $ upstreamInterfaces

validateScalarDefinition
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.ScalarTypeDefinition
  -> G.ScalarTypeDefinition
  -> m ()
validateScalarDefinition providedScalar upstreamScalar = do
  validateDirectives providedDirectives upstreamDirectives $ (Scalar, providedName)
  where
    G.ScalarTypeDefinition _ providedName providedDirectives = providedScalar

    G.ScalarTypeDefinition _ upstreamName upstreamDirectives = upstreamScalar

validateScalarDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
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
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.UnionTypeDefinition
  -> G.UnionTypeDefinition
  -> m ()
validateUnionDefinition providedUnion upstreamUnion = do
  validateDirectives providedDirectives upstreamDirectives $ (Union, providedName)
  when (S.empty /= memberTypesDiff) $ do
    refute $ pure $ NonExistingUnionMemberTypes providedName memberTypesDiff
  where
    G.UnionTypeDefinition _ providedName providedDirectives providedMemberTypes = providedUnion

    G.UnionTypeDefinition _ _ upstreamDirectives upstreamMemberTypes = upstreamUnion

    memberTypesDiff = getDifference providedMemberTypes upstreamMemberTypes

validateUnionTypeDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
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
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.ObjectTypeDefinition
  -> G.ObjectTypeDefinition
  -> S.HashSet G.Name -- ^ Interfaces declared by in the role-based schema
  -> m ()
validateObjectDefinition providedObj upstreamObj interfacesDeclared = do
  validateDirectives providedDirectives upstreamDirectives $ (Object, providedName)
  validateFieldDefinitions providedFldDefnitions upstreamFldDefnitions $ (ObjectField, providedName)
  onJust (NE.nonEmpty $ S.toList customInterfaces) $ \ifaces ->
    dispute $ pure $ CustomInterfacesNotAllowed providedName ifaces
  onJust (NE.nonEmpty nonExistingInterfaces) $ \ifaces ->
    dispute $ pure $ ObjectImplementsNonExistingInterfaces providedName ifaces
  where
    G.ObjectTypeDefinition _ providedName
       providedIfaces providedDirectives providedFldDefnitions = providedObj

    G.ObjectTypeDefinition _ _
       upstreamIfaces upstreamDirectives upstreamFldDefnitions = upstreamObj

    interfacesDiff = getDifference providedIfaces upstreamIfaces

    providedIfacesSet = S.fromList providedIfaces

    customInterfaces = S.intersection providedIfacesSet interfacesDeclared

    nonExistingInterfaces = S.toList $ S.difference providedIfacesSet interfacesDiff

validateObjectTypeDefinitions
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => [G.ObjectTypeDefinition]
  -> [G.ObjectTypeDefinition]
  -> S.HashSet G.Name
  -> m ()
validateObjectTypeDefinitions providedObjects upstreamObjects providedInterfaces = do
  flip traverse_ providedObjects $ \providedObject@(G.ObjectTypeDefinition _ name _ _ _) -> do
    upstreamObject <-
      onNothing (Map.lookup name upstreamObjectsMap) $
        refute $ pure $ FieldDoesNotExist Object name
    validateObjectDefinition providedObject upstreamObject providedInterfaces
  where
    upstreamObjectsMap = mapFromL G._otdName $ upstreamObjects

-- For the love of god, This function needs to be refactored :(
validateRemoteSchema
  :: (MonadValidate [CustomRemoteSchemaValidationError] m)
  => G.SchemaDocument
  -> G.SchemaIntrospection
  -> m G.SchemaIntrospection
validateRemoteSchema (G.SchemaDocument typeSystemDefinitions) x@(G.SchemaIntrospection originalTypeDefns) = do
  let
    (_, typeDefns) = flip runState emptySchemaDocTypeDefinitions $
                      traverse resolveTypeSystemDefinitions typeSystemDefinitions
    objDefs = _sdtdObjects typeDefns
    possibleTypesMap = createPossibleTypesMap objDefs
    interfaceDefsWithPossibleTypes = map (\iface -> -- UGH!
                                            let name = G._itdName iface
                                            in
                                            iface
                                              {G._itdPossibleTypes =
                                                 fromMaybe [] (Map.lookup name possibleTypesMap)})
  return x
  where
    -- Construction of the `possibleTypes` map for interfaces, while parsing the
    -- user provided Schema document, it doesn't include the `possibleTypes`, so
    -- constructing here, manually.
    createPossibleTypesMap :: [G.ObjectTypeDefinition] -> HashMap G.Name [G.Name]
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

    resolveTypeDefinition :: G.TypeDefinition () -> State SchemaDocumentTypeDefinitions ()
    resolveTypeDefinition (G.TypeDefinitionScalar scalarDefn) =
      modify (\td -> td {_sdtdScalars = ((:) scalarDefn) . _sdtdScalars $ td})
    resolveTypeDefinition (G.TypeDefinitionObject objectDefn) =
      modify (\td -> td {_sdtdObjects = ((:) objectDefn) . _sdtdObjects $ td})
    resolveTypeDefinition (G.TypeDefinitionInterface interfaceDefn) =
      modify (\td -> td {_sdtdInterfaces = ((:) interfaceDefn) . _sdtdInterfaces $ td})
    resolveTypeDefinition (G.TypeDefinitionUnion unionDefn) =
      modify (\td -> td {_sdtdUnions = ((:) unionDefn) . _sdtdUnions $ td})
    resolveTypeDefinition (G.TypeDefinitionEnum enumDefn) =
      modify (\td -> td {_sdtdEnums = ((:) enumDefn) . _sdtdEnums $ td})
    resolveTypeDefinition (G.TypeDefinitionInputObject inputObjectDefn) =
      modify (\td -> td {_sdtdInputObjects = ((:) inputObjectDefn) . _sdtdInputObjects $ td})

    resolveTypeSystemDefinitions :: G.TypeSystemDefinition -> State SchemaDocumentTypeDefinitions ()
    resolveTypeSystemDefinitions (G.TypeSystemDefinitionSchema schemaDefn) =
      modify (\td -> td {_sdtdSchemaDef = ((:) schemaDefn) . _sdtdSchemaDef $ td})
    resolveTypeSystemDefinitions (G.TypeSystemDefinitionType typeDefn) =
      resolveTypeDefinition typeDefn

    -- resolveSchemaIntroSpection :: G.TypeDefinition [G.Name] -> State SchemaDocumentTypeDefinitions ()
    -- resolveSchemaIntroSpection typeDef = resolveTypeDefinition $ typeDef $> ()

    emptySchemaDocTypeDefinitions = SchemaDocumentTypeDefinitions [] [] [] [] [] [] []
