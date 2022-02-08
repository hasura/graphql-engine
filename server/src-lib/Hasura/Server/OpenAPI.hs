{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
--   a `RemoteSchemaIntrospection` result generated from the `SchemaCache`.
--
-- Response bodies are mostly delcared inline, since the associated query will likely be unique and determine the fields
--   contained in the response.
module Hasura.Server.OpenAPI (serveJSON) where

import Control.Lens
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.List.NonEmpty qualified as NE
import Data.OpenApi
import Data.OpenApi.Declare
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated)
import Data.Text.NonEmpty
import Hasura.GraphQL.Analyse (Analysis (Analysis, _aFields, _aVars), FieldAnalysis (FieldAnalysis, _fFields), FieldDef (FieldInfo, FieldList), analyzeGraphqlQuery)
import Hasura.GraphQL.RemoteServer (getSchemaIntrospection)
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Media.MediaType ((//))

data EndpointData = EndpointData
  { _edUrl :: String,
    _edMethod :: [Text],
    _edVarList :: [Referenced Param],
    _edProperties :: InsOrdHashMap Text (Referenced Schema),
    _edResponse :: Maybe Response,
    _edDescription :: Text, -- contains API comments and graphql query
    _edName :: Text
  }
  deriving (Show)

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
    RemoteSchemaIntrospection
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

mkResponse :: [Text] -> String -> Maybe RemoteSchemaIntrospection -> Analysis G.Name -> Declare (Definitions Schema) (Maybe Response)
mkResponse _ _ Nothing _ = pure Nothing
mkResponse epMethods epUrl (Just rs) Analysis {..} = do
  fs <- getSelectionSchema rs (OMap.toList _aFields)
  pure $
    Just $
      mempty
        & content .~ OMap.singleton ("application" // "json") (mempty & schema ?~ Inline fs)
        & description .~ "Responses for " <> commaSeparated epMethods <> " " <> T.pack epUrl

getSelectionSchema :: RemoteSchemaIntrospection -> [(G.Name, (FieldDef, Maybe (FieldAnalysis var)))] -> Declare (Definitions Schema) Schema
getSelectionSchema rs fields = do
  ps <- traverse (pure . G.unName . fst &&&& (\(fN, (td, fA)) -> getDefinitionSchema rs fN td fA {- (\(fN,(td,fA)) -> pure $ (G.unName fN,) $ getDefinitionSchema rs td fA) -})) fields
  pure $ mempty & properties .~ OMap.fromList (map (second Inline) ps)

-- | A helper function to set the pattern field in Schema
--   Why not lens `pattern`? hlint doesn't like the name `pattern`
--   https://github.com/ndmitchell/hlint/issues/607
setPattern :: Maybe Pattern -> Schema -> Schema
setPattern p s = s {_schemaPattern = p}

getDefinitionSchema ::
  RemoteSchemaIntrospection ->
  G.Name ->
  FieldDef ->
  Maybe (FieldAnalysis var) ->
  Declare (Definitions Schema) Schema
getDefinitionSchema rs tn fd fA =
  typeToSchemaM
    fd
    ( \case
        (G.TypeDefinitionInterface _) -> pure $ mempty & description ?~ "Unsupported field type TypeDefinitionInterface: " <> G.unName tn
        (G.TypeDefinitionUnion _) -> pure $ mempty & description ?~ "Unsupported field type TypeDefinitionUnion: " <> G.unName tn
        (G.TypeDefinitionEnum _) -> pure $ mempty & description ?~ "Unsupported field type TypeDefinitionEnum: " <> G.unName tn
        (G.TypeDefinitionInputObject _) -> pure $ mempty & description ?~ "Unsupported field type TypeDefinitionInputObject: " <> G.unName tn
        (G.TypeDefinitionObject _) ->
          case fA of
            Nothing -> pure $ mempty & description ?~ "Field analysis not found"
            Just FieldAnalysis {..} -> do
              ps <- traverse (pure . G.unName . fst &&&& (\(fN, (td', fA')) -> getDefinitionSchema rs fN td' fA')) (OMap.toList _fFields)
              pure $
                mempty
                  & properties .~ OMap.fromList (map (second Inline) ps)
                  & type_ ?~ OpenApiObject
        (G.TypeDefinitionScalar std) -> do
          let (refType, patt) = referenceType True (T.toLower $ G.unName $ G._stdName std)
          pure $
            mempty
              & title ?~ G.unName (G._stdName std)
              & description .~ (G.unDescription <$> G._stdDescription std)
              & type_ .~ refType
              & setPattern patt
    )

typeToSchemaM :: Monad m => FieldDef -> (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition -> m Schema) -> m Schema
typeToSchemaM (FieldInfo _nullability tName) k = k tName
typeToSchemaM (FieldList n t) k = do
  t' <- typeToSchemaM t k
  pure $
    mempty
      & nullable ?~ G.unNullability n
      & type_ ?~ OpenApiArray
      & items ?~ OpenApiItemsObject (Inline t') -- TODO: Why do we assume objects here?

infixl 7 &&&&

(&&&&) :: Applicative f => (t -> f a1) -> (t -> f a2) -> t -> f (a1, a2)
f &&&& g = \a -> (,) <$> f a <*> g a

-- * URL / Query Params and Request Body Functions

-- There could be an additional partitioning scheme besides referentiality to support more types in Params
getParams :: Analysis G.Name -> EndpointUrl -> [Referenced Param]
getParams Analysis {..} eURL = varDetails =<< Map.toList _aVars
  where
    pathVars = map T.tail $ concat $ splitPath pure (const []) eURL -- NOTE: URL Variable name ':' prefix is removed for `elem` lookup.
    varDetails (_vdName, (_vdType, _vdDefaultValue)) =
      let vName = G.unName _vdName
          isRequired = not $ G.isNullable _vdType
       in case getType _vdType of
            Left _foo -> [] -- Complex types are not allowed as params
            Right (vdType, patt) ->
              pure $
                Inline $
                  mkParam
                    vName
                    (if isRequired then Just $ "_\"" <> vName <> "\" is required (enter it either in parameters or request body)_" else Nothing)
                    Nothing
                    (if vName `elem` pathVars then ParamPath else ParamQuery)
                    Nothing
                    (gqlToJsonValue <$> _vdDefaultValue)
                    (Just vdType)
                    patt

getType :: G.GType -> Either G.GType (OpenApiType, Maybe Pattern)
getType gt@(G.TypeNamed _ na) = case referenceType True t of
  (Nothing, _) -> Left gt
  (Just typ, patt) -> Right (typ, patt)
  where
    t = T.toLower $ G.unName na
getType t = Left t -- Non scalar types are deferred to reference types for processing using introspection

mkProperties :: Maybe RemoteSchemaIntrospection -> Analysis G.Name -> Declare (Definitions Schema) (InsOrdHashMap Text (Referenced Schema))
mkProperties sd Analysis {..} = OMap.fromList <$> traverse (mkProperty sdMap) ds
  where
    ds = Map.toList _aVars
    sdMap = case sd of
      Nothing -> OMap.empty
      Just (RemoteSchemaIntrospection sd') -> OMap.fromList $ map (first G.unName) $ Map.toList sd'

mkProperty :: InsOrdHashMap Text (G.TypeDefinition [G.Name] RemoteSchemaInputValueDefinition) -> (G.Name, (G.GType, Maybe (G.Value Void))) -> Declare (Definitions Schema) (Text, Referenced Schema)
mkProperty sd (_vdName, (_vdType, _vdDefaultValue)) = do
  d <- case getType _vdType of
    Left t -> handleRefType sd t
    Right (vdType, patt) ->
      pure $
        Inline $
          mempty
            & nullable ?~ G.isNullable _vdType
            & type_ ?~ vdType
            & default_ .~ fmap gqlToJsonValue _vdDefaultValue
            & setPattern patt

  pure (G.unName _vdName, d)

handleRefType :: OMap.InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> G.GType -> Declare (Definitions Schema) (Referenced Schema)
handleRefType sd = \case
  G.TypeNamed nullability nameWrapper -> do
    let n = G.unName nameWrapper
    declareReference nullability n sd
    pure $ Ref $ Reference n
  G.TypeList nullability subType -> do
    st <- handleRefType sd subType
    pure $
      Inline $
        mempty
          & nullable ?~ (G.unNullability nullability && G.isNullable subType)
          & type_ ?~ OpenApiArray
          & items ?~ OpenApiItemsObject st

-- TODO: No reference types should be nullable and only references to reference types

declareReference :: G.Nullability -> Text -> OMap.InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> Declare (Definitions Schema) ()
declareReference nullability n ts = do
  isAvailable <- referenceAvailable n
  unless isAvailable do
    for_ (OMap.lookup n ts) \t -> do
      let properties' = getPropertyReferences ts (typeProperties t)

      result <-
        declare $
          OMap.singleton n $
            let (refType, patt) = referenceType (null properties') (T.toLower n)
             in mempty
                  & nullable ?~ G.unNullability nullability
                  & description .~ typeDescription t
                  & properties .~ properties'
                  & type_ .~ refType
                  & setPattern patt
      void $ processProperties ts (typeProperties t)
      pure result

referenceAvailable :: Text -> DeclareT (Definitions Schema) Identity Bool
referenceAvailable n = OMap.member n <$> look

getPropertyReferences :: InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> Maybe [RemoteSchemaInputValueDefinition] -> InsOrdHashMap Text (Referenced Schema)
getPropertyReferences _ Nothing = mempty
getPropertyReferences sd (Just ds) =
  let ds' = fmap (processProperty' sd) ds
   in OMap.fromList ds'

processProperty' :: InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> RemoteSchemaInputValueDefinition -> (Text, Referenced Schema)
processProperty' sd (RemoteSchemaInputValueDefinition d _preset) =
  let n = G._ivdName d
      t = G._ivdType d
      rt = handleRefType' sd t
   in (G.unName n, rt)

handleRefType' :: OMap.InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> G.GType -> Referenced Schema
handleRefType' sd = \case
  G.TypeNamed _nullability nameWrapper ->
    let n = G.unName nameWrapper
     in Ref $ Reference n
  G.TypeList nullability subType ->
    let st = handleRefType' sd subType
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

typeProperties :: G.TypeDefinition possibleTypes RemoteSchemaInputValueDefinition -> Maybe [RemoteSchemaInputValueDefinition]
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

processProperties :: InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> Maybe [RemoteSchemaInputValueDefinition] -> DeclareT (Definitions Schema) Identity (InsOrdHashMap Text (Referenced Schema))
processProperties _ Nothing = pure mempty
processProperties sd (Just ds) = do
  ds' <- traverse (processProperty sd) ds
  pure $ OMap.fromList ds'

processProperty :: InsOrdHashMap Text (G.TypeDefinition a RemoteSchemaInputValueDefinition) -> RemoteSchemaInputValueDefinition -> DeclareT (Definitions Schema) Identity (Text, Referenced Schema)
processProperty sd (RemoteSchemaInputValueDefinition d _preset) = do
  let n = G._ivdName d
      t = G._ivdType d
  rt <- handleRefType sd t
  pure (G.unName n, rt)

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
            & setPattern patt
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

extractEndpointInfo :: Maybe RemoteSchemaIntrospection -> EndpointMethod -> EndpointMetadata GQLQueryWithText -> Declare (Definitions Schema) EndpointData
extractEndpointInfo sd method d = do
  _edProperties <- mkProperties sd _analysis
  _edResponse <- mkResponse _edMethod _edUrl sd _analysis
  pure EndpointData {..}
  where
    _eDef = mdDefinitions d
    -- mdDefinition returns a list, but there should only be one definition associated, so it is safe to fold
    _analysis = fromMaybe mempty (fold $ mapMaybe (\e -> fmap (analyzeGraphqlQuery e) sd) _eDef)
    _edUrl = T.unpack . getURL $ d
    _edVarList = getParams _analysis (_ceUrl d)
    _edDescription = getComment d
    _edName = unNonEmptyText $ unEndpointName $ _ceName d
    _edMethod = [unEndpointMethod method] -- NOTE: Methods are grouped with into matching endpoints - Name used for grouping.

getEndpointsData :: Maybe RemoteSchemaIntrospection -> SchemaCache -> Declare (Definitions Schema) [EndpointData]
getEndpointsData sd sc = do
  let endpointTrie = scEndpoints sc
      methodMaps = leaves endpointTrie
      endpointsWithMethods = concatMap (\(m, s) -> map (m,) (Set.toList s)) $ concatMap (Map.toList . _unMultiMap) methodMaps

  endpointsWithInfo <- traverse (uncurry (extractEndpointInfo sd)) endpointsWithMethods

  let endpointsGrouped = NE.groupBy (\a b -> _edName a == _edName b) endpointsWithInfo

  pure $ map squashEndpointGroup endpointsGrouped

squashEndpointGroup :: NonEmpty EndpointData -> EndpointData
squashEndpointGroup g = (NE.head g) {_edMethod = concatMap _edMethod g}

serveJSON :: SchemaCache -> OpenApi
serveJSON sc = spec & components . schemas .~ defs
  where
    (defs, spec) = runDeclare (declareOpenApiSpec sc) mempty

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

declareOpenApiSpec :: SchemaCache -> Declare (Definitions Schema) OpenApi
declareOpenApiSpec sc = do
  let _schemaIntrospection = getSchemaIntrospection (scGQLContext sc)
      warnings = case _schemaIntrospection of
        Nothing -> "\n\n⚠️ Schema introspection failed"
        _ -> ""

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

      mkOperation :: EndpointData -> Operation
      mkOperation ed =
        mempty
          & description ?~ _edDescription ed
          & summary ?~ _edName ed
          & parameters .~ (Inline xHasuraAdminSecret : _edVarList ed)
          & requestBody .~ toMaybe (not (null (_edProperties ed))) (Inline (mkRequestBody ed))
          & responses .~ Responses Nothing (maybe mempty (OMap.singleton 200 . Inline) (_edResponse ed))
        where
          toMaybe b a = if b then Just a else Nothing

      getOPName :: EndpointData -> Text -> Maybe Operation
      getOPName ed methodType =
        if methodType `elem` _edMethod ed
          then Just $ mkOperation ed
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
                & get .~ getOPName ed "GET"
                & post .~ getOPName ed "POST"
                & put .~ getOPName ed "PUT"
                & delete .~ getOPName ed "DELETE"
                & patch .~ getOPName ed "PATCH"
            completePathData =
              if pathData == mempty
                then
                  mempty
                    & post
                      ?~ mkOperation
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

  return $
    mempty
      & paths .~ openAPIPaths
      & info . title .~ "Rest Endpoints"
      & info . description ?~ "This OpenAPI specification is automatically generated by Hasura." <> warnings
