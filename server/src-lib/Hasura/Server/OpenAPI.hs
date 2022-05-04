{-# LANGUAGE ViewPatterns #-}
-- This prevents hlint errors on the "pattern" lens.
{-# LANGUAGE NoPatternSynonyms #-}

-- |
-- Module      : Hasura.Server.OpenAPI
-- Description : Builds an OpenAPI specification for the REST endpoints from a SchemaCache via the `declareOpenApiSpec` function.
--
-- The implementation currently iterates over the endpoints building up `EndpointData` for each then exposes this as an OpenAPI Schema.
--
-- Most functions are in the `Declare` monad so that they can add new component definitions on the fly that can be referenced.
--   This is especially useful for the params and request body documentation.
--
-- The response body recurses over the SelectionSet Fields associated with an endpoint and looks up types by name in
--   a `SchemaIntrospection` result generated from the `SchemaCache`.
--
-- Response bodies are mostly delcared inline, since the associated query will likely be unique and determine the fields
--   contained in the response.
module Hasura.Server.OpenAPI (serveJSON) where

import Control.Lens
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashMap.Strict.Multi qualified as MMap
import Data.HashSet qualified as Set
import Data.List.NonEmpty qualified as NE
import Data.OpenApi
import Data.OpenApi.Declare
import Data.Text qualified as T
import Data.Text.NonEmpty
import Data.Trie qualified as Trie
import Hasura.Base.Error
import Hasura.GraphQL.Analyse
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.SchemaCache hiding (FieldInfo)
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Media.MediaType ((//))

data EndpointData = EndpointData
  { _edUrl :: String,
    _edMethod :: HashSet EndpointMethod,
    _edVarList :: [Referenced Param],
    _edProperties :: InsOrdHashMap Text (Referenced Schema),
    _edResponse :: HashMap EndpointMethod Response,
    _edDescription :: Text, -- contains API comments and graphql query
    _edName :: Text,
    _edErrs :: [Text]
  }
  deriving (Show)

-- | @DeclareErr@ is just a @ExceptT@ monad with the added capabilities of @Declare@ monad
type DeclareErr d = ExceptT QErr (Declare d)

-- * Response Body Related Functions.

{-

Example stepthrough initiated by call to getSelectionSchema:

  * Endpoint insert_foo
  * getExecutableDefinitions -> List (Normally 1 entry)
  * ExecutableDefinitionOperation -> OperationDefinitionTyped -> TypedOperationDefinition (_todType = OperationTypeMutation)
  * _todSelectionSet ->
    [SelectionField
      (Field{ _fName = Name{unName = "insert_foo"},
            _fSelectionSet
              = [SelectionField
                  (Field{ _fName = Name{unName = "returning"},
                        _fSelectionSet
                          = [..., SelectionField
                              (Field{ _fName = Name{unName = "id"},
                                    _fSelectionSet = []}),
  * Lookup introspection schema, Under mutation_root (inferred from operationtype = OperationTypeMutation)
    SchemaIntrospection
    (fromList
        [..., (Name{unName = "mutation_root"},
          TypeDefinitionObject
            (ObjectTypeDefinition{_otdDescription =
                                    Just (Description{unDescription = "mutation root"}),
                                  _otdName = Name{unName = "mutation_root"},
                                  _otdImplementsInterfaces = [], _otdDirectives = [],
                                  _otdFieldsDefinition =
                                    [..., FieldDefinition{_fldDescription = Just (Description{unDescription = "insert data into the table: \"foo\""}),
                                                    _fldName = Name{unName = "insert_foo"},
                                                    _fldType = TypeNamed (Nullability{unNullability = True}) (Name{unName = "foo_mutation_response"}),
  * Find that type for insert_foo field in mutation_root is named type "foo_mutation_response"
  * Look up "foo_mutation_response" in introspection schema:
        (Name{unName = "foo_mutation_response"},
          TypeDefinitionObject
            (ObjectTypeDefinition{_otdDescription = Just (Description{unDescription = "response of any mutation on the table \"foo\""}),
                                  _otdName = Name{unName = "foo_mutation_response"},
                                  _otdFieldsDefinition =
                                    [..., FieldDefinition{_fldDescription =
                                                      Just
                                                        (Description{unDescription =
                                                                        "data from the rows affected by the mutation"}),
                                                    _fldName = Name{unName = "returning"},
                                                    _fldArgumentsDefinition = [],
                                                    _fldType =
                                                      TypeList (Nullability{unNullability = False})
                                                        (TypeNamed
                                                            (Nullability{unNullability = False})
                                                            (Name{unName = "foo"})),
                                                    _fldDirectives = []}]})),
  * Find first referenced sub-field "returning"
  * It has type (TypeNamed (Name{unName = "foo"})),
  * Look up "foo" in Introspection Schema: ...,
        (Name{unName = "foo"},
          TypeDefinitionObject
            (ObjectTypeDefinition{_otdDescription = Just (Description{unDescription = "columns and relationships of \"foo\""}),
                                  _otdName = Name{unName = "foo"}, _otdImplementsInterfaces = [],
                                  _otdFieldsDefinition =
                                    [ ..., FieldDefinition{_fldDescription = Nothing,
                                                    _fldName = Name{unName = "id"},
                                                    _fldType = TypeNamed (Nullability{unNullability = False}) (Name{unName = "uuid"}),
  * Lookup first sub-sub field by SelectionSet field name "id"
  * See that it has type: TypeNamed (Name{unName = "uuid"})
  * See that there are no sub-sub-sub fields
  * declare type uuid by looking up its definition
        (Name{unName = "uuid"},
        TypeDefinitionScalar
          (ScalarTypeDefinition{_stdDescription = Nothing,
                                _stdName = Name{unName = "uuid"}, _stdDirectives = []})),
  * reference type name from components in output

  ... Proceed with other sub-fields and fields

-}

-- FIXME: There should only be one definition associated. Find a way to signal an error here otherwise.
mdDefinitions :: EndpointMetadata GQLQueryWithText -> [G.ExecutableDefinition G.Name]
mdDefinitions = G.getExecutableDefinitions . unGQLQuery . getGQLQuery . _edQuery . _ceDefinition

mkResponse ::
  Structure ->
  EndpointMethod ->
  String ->
  Declare (Definitions Schema) (Maybe Response)
mkResponse (Structure (Selection fields) _) epMethod epUrl = do
  fs <- getSelectionSchema (Map.toList fields)
  pure $
    Just $
      mempty
        & content .~ OMap.singleton ("application" // "json") (mempty & schema ?~ Inline fs)
        & description .~ "Responses for " <> tshow epMethod <> " " <> T.pack epUrl

getSelectionSchema ::
  [(G.Name, FieldInfo)] ->
  Declare (Definitions Schema) Schema
getSelectionSchema fields = do
  props <- for fields \(fieldName, fieldInfo) -> do
    fieldSchema <- getDefinitionSchema fieldName fieldInfo
    pure (G.unName fieldName, fieldSchema)
  pure $ mempty & properties .~ fmap Inline (OMap.fromList props)

getDefinitionSchema ::
  G.Name ->
  FieldInfo ->
  Declare (Definitions Schema) Schema
getDefinitionSchema fieldName (FieldInfo {..}) =
  _fiType & go \typeName -> case _fiTypeDefinition of
    G.TypeDefinitionInterface _ ->
      pure $
        mempty & description
          ?~ "Unsupported type interface " <> G.unName typeName <> " for field " <> G.unName fieldName
    G.TypeDefinitionUnion _ ->
      pure $
        mempty & description
          ?~ "Unsupported type union: " <> G.unName typeName <> " for field " <> G.unName fieldName
    G.TypeDefinitionEnum _ ->
      pure $
        mempty & description
          ?~ "Unsupported type enum: " <> G.unName typeName <> " for field " <> G.unName fieldName
    G.TypeDefinitionInputObject _ ->
      pure $
        mempty & description
          ?~ "Internal error! Input type " <> G.unName typeName <> " in output selection set for field " <> G.unName fieldName
    G.TypeDefinitionObject _ ->
      case _fiSelection of
        Nothing -> pure $ mempty & description ?~ "Missing selection for object type: " <> G.unName typeName
        Just (Selection fields) -> do
          objectSchema <- getSelectionSchema $ Map.toList fields
          pure $
            objectSchema
              & type_ ?~ OpenApiObject
    G.TypeDefinitionScalar std -> do
      let (refType, typePattern) = referenceType True (T.toLower $ G.unName typeName)
      pure $
        mempty
          & title ?~ G.unName typeName
          & description .~ (G.unDescription <$> G._stdDescription std)
          & type_ .~ refType
          & pattern .~ typePattern
  where
    go :: (G.Name -> Declare (Definitions Schema) Schema) -> G.GType -> Declare (Definitions Schema) Schema
    go fun = \case
      -- TODO: why do we ignore the nullability for the leaf?
      G.TypeNamed _nullability typeName -> fun typeName
      G.TypeList (G.Nullability isNullable) innerType -> do
        result <- go fun innerType
        pure $
          mempty
            & nullable ?~ isNullable
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (Inline result) -- TODO: Why do we assume objects here?

-- * URL / Query Params and Request Body Functions

-- There could be an additional partitioning scheme besides referentiality to support more types in Params
getParams :: Structure -> EndpointUrl -> [Referenced Param]
getParams (Structure _fields vars) eURL = do
  (G.unName -> varName, varInfo) <- Map.toList vars
  -- Complex types are not allowed as params
  case getType $ _viType varInfo of
    Left _ -> []
    Right (refType, typePattern) -> do
      let isRequired = not $ G.isNullable $ _viType varInfo
      pure $
        Inline $
          mkParam
            varName
            (if isRequired then Just $ "_\"" <> varName <> "\" is required (enter it either in parameters or request body)_" else Nothing)
            Nothing
            (if varName `elem` pathVars then ParamPath else ParamQuery)
            Nothing
            (gqlToJsonValue <$> _viDefaultValue varInfo)
            (Just refType)
            typePattern
  where
    pathVars = map T.tail $ concat $ splitPath pure (const []) eURL -- NOTE: URL Variable name ':' prefix is removed for `elem` lookup.

getType :: G.GType -> Either G.GType (OpenApiType, Maybe Pattern)
getType gt@(G.TypeNamed _ na) = case referenceType True t of
  (Nothing, _) -> Left gt
  (Just typ, patt) -> Right (typ, patt)
  where
    t = T.toLower $ G.unName na
getType t = Left t -- Non scalar types are deferred to reference types for processing using introspection

mkProperties ::
  G.SchemaIntrospection ->
  Structure ->
  Declare (Definitions Schema) (InsOrdHashMap Text (Referenced Schema))
mkProperties schemaTypes Structure {..} =
  OMap.fromList
    <$> for (Map.toList _stVariables) \(varName, varInfo) -> do
      property <- mkProperty schemaTypes varInfo
      pure (G.unName varName, property)

mkProperty ::
  G.SchemaIntrospection ->
  VariableInfo ->
  Declare (Definitions Schema) (Referenced Schema)
mkProperty schemaTypes VariableInfo {..} =
  case getType _viType of
    Left t -> handleRefType schemaTypes t
    Right (refType, typePattern) ->
      pure $
        Inline $
          mempty
            & nullable ?~ G.isNullable _viType
            & type_ ?~ refType
            & default_ .~ fmap gqlToJsonValue _viDefaultValue
            & pattern .~ typePattern

handleRefType ::
  G.SchemaIntrospection ->
  G.GType ->
  Declare (Definitions Schema) (Referenced Schema)
handleRefType schemaTypes = \case
  G.TypeNamed nullability varName -> do
    declareReference schemaTypes nullability varName
    pure $ Ref $ Reference $ G.unName varName
  G.TypeList nullability subType -> do
    st <- handleRefType schemaTypes subType
    pure $
      Inline $
        mempty
          -- TODO: is that correct? we do something different for objects.
          & nullable ?~ (G.unNullability nullability && G.isNullable subType)
          & type_ ?~ OpenApiArray
          & items ?~ OpenApiItemsObject st

-- TODO: No reference types should be nullable and only references to reference types

declareReference ::
  G.SchemaIntrospection ->
  G.Nullability ->
  G.Name ->
  Declare (Definitions Schema) ()
declareReference schemaTypes@(G.SchemaIntrospection typeDefinitions) nullability refName = do
  isAvailable <- referenceAvailable $ G.unName refName
  unless isAvailable $
    for_ (Map.lookup refName typeDefinitions) \typeDefinition -> do
      let properties' = getPropertyReferences $ typeProperties typeDefinition
          (refType, typePattern) = referenceType (null properties') (T.toLower $ G.unName refName)
      declare $
        OMap.singleton (G.unName refName) $
          mempty
            & nullable ?~ G.unNullability nullability
            & description .~ typeDescription typeDefinition
            & properties .~ properties'
            & type_ .~ refType
            & pattern .~ typePattern
      void $ processProperties schemaTypes $ typeProperties typeDefinition

referenceAvailable :: Text -> DeclareT (Definitions Schema) Identity Bool
referenceAvailable n = OMap.member n <$> look

getPropertyReferences :: Maybe [G.InputValueDefinition] -> InsOrdHashMap Text (Referenced Schema)
getPropertyReferences = \case
  Nothing -> mempty
  Just definitions ->
    OMap.fromList $
      definitions <&> \G.InputValueDefinition {..} ->
        (G.unName $ _ivdName, handleRefType' _ivdType)

handleRefType' :: G.GType -> Referenced Schema
handleRefType' = \case
  G.TypeNamed _nullability nameWrapper ->
    let n = G.unName nameWrapper
     in Ref $ Reference n
  G.TypeList nullability subType ->
    let st = handleRefType' subType
     in Inline $
          mempty
            & nullable ?~ (G.unNullability nullability && G.isNullable subType)
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject st

typeDescription :: G.TypeDefinition possibleTypes inputType -> Maybe Text
typeDescription = \case
  (G.TypeDefinitionScalar o) -> G.unDescription <$> G._stdDescription o
  (G.TypeDefinitionObject o) -> G.unDescription <$> G._otdDescription o
  (G.TypeDefinitionInterface o) -> G.unDescription <$> G._itdDescription o
  (G.TypeDefinitionUnion o) -> G.unDescription <$> G._utdDescription o
  (G.TypeDefinitionEnum o) -> G.unDescription <$> G._etdDescription o
  (G.TypeDefinitionInputObject o) -> G.unDescription <$> G._iotdDescription o

typeProperties :: G.TypeDefinition possibleTypes G.InputValueDefinition -> Maybe [G.InputValueDefinition]
typeProperties = \case
  (G.TypeDefinitionScalar _) -> Nothing
  (G.TypeDefinitionInterface _) -> Nothing
  (G.TypeDefinitionUnion _) -> Nothing
  (G.TypeDefinitionEnum _) -> Nothing
  (G.TypeDefinitionInputObject o) -> Just $ G._iotdValueDefinitions o
  (G.TypeDefinitionObject _) -> Nothing

-- TODO: Can we reuse something from rest module to handle this?
-- TODO: referenceType could be improved, instead of using Bool (to indicate if it is object or scalar),
--        we can do something better
referenceType :: Bool -> Text -> (Maybe OpenApiType, Maybe Pattern)
referenceType False = const (Just OpenApiObject, Nothing)
referenceType True = \case
  "int" -> (Just OpenApiInteger, Nothing)
  "float" -> (Just OpenApiNumber, Nothing)
  "double" -> (Just OpenApiNumber, Nothing)
  "uuid" -> (Just OpenApiString, Just "[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89aAbB][a-f0-9]{3}-[a-f0-9]{12}")
  "bool" -> (Just OpenApiBoolean, Nothing)
  "boolean" -> (Just OpenApiBoolean, Nothing)
  "string" -> (Just OpenApiString, Nothing)
  "id" -> (Just OpenApiString, Nothing)
  _ -> (Nothing, Nothing)

processProperties ::
  G.SchemaIntrospection ->
  Maybe [G.InputValueDefinition] ->
  DeclareT (Definitions Schema) Identity (InsOrdHashMap Text (Referenced Schema))
processProperties schemaTypes = \case
  Nothing -> pure mempty
  Just definitions ->
    OMap.fromList <$> for definitions \G.InputValueDefinition {..} -> do
      property <- handleRefType schemaTypes _ivdType
      pure (G.unName $ _ivdName, property)

getGQLQueryFromTrie :: EndpointMetadata GQLQueryWithText -> Text
getGQLQueryFromTrie = getGQLQueryText . _edQuery . _ceDefinition

mkParam :: Text -> Maybe Text -> Maybe Bool -> ParamLocation -> Maybe Bool -> Maybe J.Value -> Maybe OpenApiType -> Maybe Pattern -> Param
mkParam nameP desc req loc allowEmpty def varType patt =
  mempty
    & name .~ nameP
    & description .~ desc
    & required .~ req
    & in_ .~ loc
    & allowEmptyValue .~ allowEmpty
    & schema
      ?~ Inline
        ( mempty
            & default_ .~ def
            & type_ .~ varType
            & pattern .~ patt
        )

gqlToJsonValue :: G.Value Void -> J.Value
gqlToJsonValue = \case
  G.VNull -> J.Null
  G.VInt n -> J.toJSON n
  G.VFloat sci -> J.toJSON sci
  G.VString txt -> J.toJSON txt
  G.VBoolean b -> J.toJSON b
  G.VEnum ev -> J.toJSON ev
  G.VList lst -> J.toJSON $ gqlToJsonValue <$> lst
  G.VObject obj -> J.toJSON $ gqlToJsonValue <$> obj

-- * Top level schema construction

getComment :: EndpointMetadata GQLQueryWithText -> Text
getComment d = comment
  where
    gql = getGQLQueryFromTrie d
    comment = case _ceComment d of
      (Just c) -> c <> "\n***\nThe GraphQl query for this endpoint is:\n``` graphql\n" <> gql <> "\n```"
      Nothing -> "***\nThe GraphQl query for this endpoint is:\n``` graphql\n" <> gql <> "\n```"

getURL :: EndpointMetadata GQLQueryWithText -> Text
getURL d =
  "/api/rest/" <> T.intercalate "/" pathComponents
  where
    pathComponents = splitPath formatVariable id . _ceUrl $ d
    formatVariable variable = "{" <> dropColonPrefix variable <> "}"
    dropColonPrefix = T.drop 1

extractEndpointInfo :: G.SchemaIntrospection -> EndpointMethod -> EndpointMetadata GQLQueryWithText -> DeclareErr (Definitions Schema) EndpointData
extractEndpointInfo schemaTypes method d = do
  singleOperation <- getSingleOperation (GQLReq Nothing (GQLExecDoc (mdDefinitions d)) Nothing)
  let (analysis, allErrors) = analyzeGraphQLQuery schemaTypes singleOperation
      _edUrl = T.unpack . getURL $ d
      _edVarList = getParams analysis (_ceUrl d)
      _edDescription = getComment d
      _edName = unNonEmptyText $ unEndpointName $ _ceName d
      _edMethod = Set.singleton method -- NOTE: Methods are grouped with into matching endpoints - Name used for grouping.
      _edErrs = allErrors
  _edProperties <- lift $ mkProperties schemaTypes analysis
  _edResponse <- lift $ foldMap (Map.singleton method) <$> mkResponse analysis method _edUrl

  pure $ EndpointData {..}

getEndpointsData ::
  G.SchemaIntrospection ->
  SchemaCache ->
  DeclareErr (Definitions Schema) [EndpointData]
getEndpointsData sd sc = do
  let endpointTrie = scEndpoints sc
      methodMaps = Trie.elems endpointTrie
      endpointsWithMethods = concatMap (\(m, s) -> map (m,) s) $ concatMap MMap.toList methodMaps

  endpointsWithInfo <- traverse (uncurry (extractEndpointInfo sd)) endpointsWithMethods

  let endpointsGrouped = NE.groupBy (\a b -> _edName a == _edName b) endpointsWithInfo

  pure $ map squashEndpointGroup endpointsGrouped

squashEndpointGroup :: NonEmpty EndpointData -> EndpointData
squashEndpointGroup g =
  (NE.head g)
    { _edMethod = foldMap _edMethod g,
      _edResponse = foldMap _edResponse g
    }

serveJSON :: (MonadError QErr m) => SchemaCache -> m OpenApi
serveJSON sc =
  let (defs, spec) = flip runDeclare mempty . runExceptT $ declareOpenApiSpec sc
   in case spec of
        Left qe -> throwError qe
        Right openAPISpec -> pure $ openAPISpec & components . schemas .~ defs

-- | If all variables are scalar or optional then the entire request body can be marked as optional
isRequestBodyRequired :: EndpointData -> Bool
isRequestBodyRequired ed = not $ all isNotRequired (_edProperties ed)
  where
    -- The use of isNotRequired here won't work with list types since they are inline, but contain references
    isNotRequired (Inline Schema {..}) = isScalarType _schemaType || (Just True == _schemaNullable)
    isNotRequired (Ref _) = False -- Not all `Ref` are non nullable, imagine two endpoints using the same Ref one being nullable and other not
    isScalarType :: Maybe OpenApiType -> Bool
    isScalarType Nothing = False
    isScalarType (Just t) = case t of
      OpenApiString -> True
      OpenApiNumber -> True
      OpenApiInteger -> True
      OpenApiBoolean -> True
      OpenApiArray -> False
      OpenApiNull -> False
      OpenApiObject -> False

-- * Entry point

declareOpenApiSpec :: SchemaCache -> DeclareErr (Definitions Schema) OpenApi
declareOpenApiSpec sc = do
  let _schemaIntrospection = scAdminIntrospection sc
      mkRequestBody :: EndpointData -> RequestBody
      mkRequestBody ed =
        mempty
          & description ?~ "Query parameters can also be provided in the request body as a JSON object"
          & required ?~ isRequestBodyRequired ed
          & content
            .~ OMap.singleton
              ("application" // "json")
              ( mempty
                  & schema
                    ?~ Inline
                      ( mempty
                          & type_ ?~ OpenApiObject
                          & properties .~ _edProperties ed
                      )
              )

      mkOperation :: EndpointMethod -> EndpointData -> Operation
      mkOperation method ed =
        mempty
          & description ?~ _edDescription ed
          & summary ?~ _edName ed
          & parameters .~ (Inline xHasuraAdminSecret : _edVarList ed)
          & requestBody .~ toMaybe (not (null (_edProperties ed))) (Inline (mkRequestBody ed))
          & responses .~ Responses Nothing (maybe mempty (OMap.singleton 200 . Inline) $ Map.lookup method $ _edResponse ed)
        where
          toMaybe b a = if b then Just a else Nothing

      getOPName :: EndpointData -> EndpointMethod -> Maybe Operation
      getOPName ed methodType =
        if methodType `Set.member` _edMethod ed
          then Just $ mkOperation methodType ed
          else Nothing

      xHasuraAdminSecret :: Param
      xHasuraAdminSecret =
        mkParam
          "x-hasura-admin-secret"
          (Just "Your x-hasura-admin-secret will be used for authentication of the API request.")
          Nothing
          ParamHeader
          Nothing
          Nothing
          (Just OpenApiString)
          Nothing

      generatePathItem :: EndpointData -> PathItem
      generatePathItem ed =
        let pathData =
              mempty
                & get .~ getOPName ed GET
                & post .~ getOPName ed POST
                & put .~ getOPName ed PUT
                & delete .~ getOPName ed DELETE
                & patch .~ getOPName ed PATCH
            completePathData =
              if pathData == mempty
                then
                  mempty
                    & post
                      ?~ mkOperation
                        POST
                        ed
                          { _edDescription =
                              "⚠️ Method("
                                <> tshow (_edMethod ed)
                                <> ") not supported, defaulting to POST\n\n"
                                <> _edDescription ed
                          }
                else pathData
         in completePathData

  endpointLst <- getEndpointsData _schemaIntrospection sc

  let mkOpenAPISchema :: [EndpointData] -> InsOrdHashMap FilePath PathItem
      mkOpenAPISchema edLst = foldl (\hm ed -> OMap.insertWith (<>) (_edUrl ed) (generatePathItem ed) hm) mempty edLst

      openAPIPaths = mkOpenAPISchema endpointLst

      allWarnings = foldl addEndpointWarnings "" endpointLst
      addEndpointWarnings :: Text -> EndpointData -> Text
      addEndpointWarnings oldWarn EndpointData {..} =
        if null _edErrs
          then oldWarn
          else
            oldWarn <> "\n\nEndpoint \""
              <> _edName
              <> "\":"
              <> foldl (\w err -> w <> "\n- ⚠️ " <> err) "" _edErrs

  return $
    mempty
      & paths .~ openAPIPaths
      & info . title .~ "Rest Endpoints"
      & info . description ?~ "This OpenAPI specification is automatically generated by Hasura." <> allWarnings
