{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.OpenAPI (serveJSON) where

import Control.Lens
import Data.Aeson (Value, toJSON)
import Data.HashMap.Strict qualified as M
import Data.HashMap.Strict.InsOrd qualified as MI
import Data.List.NonEmpty qualified as LNE
import Data.OpenApi
import Data.OpenApi.Declare
import Data.Set.Internal qualified as S
import Data.Text qualified as T
import Data.Text.NonEmpty qualified as TNE
import Hasura.Prelude hiding (get, put)
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.SchemaCache
import Language.GraphQL.Draft.Syntax qualified as G

data EndpointData = EndpointData
  { _edUrl :: String,
    _edMethod :: [Text],
    _edVarList :: [Referenced Param],
    _edDescription :: Text, -- contains API comments and graphql query
    _edName :: Text
  }

getVarList :: EndpointMetadata GQLQueryWithText -> [G.VariableDefinition]
getVarList e = vars =<< varLists
  where
    varLists = G.getExecutableDefinitions . unGQLQuery . getGQLQuery . _edQuery . _ceDefinition $ e
    vars x = case x of
      G.ExecutableDefinitionOperation (G.OperationDefinitionTyped (G.TypedOperationDefinition _ _ vds _ _)) -> vds
      _ -> []

getVariableDefinitions :: EndpointMetadata GQLQueryWithText -> [Referenced Param]
getVariableDefinitions d = fmap varDetails varList
  where
    pathVars = map T.tail $ lefts $ splitPath Left Right (_ceUrl d) -- NOTE: URL Variable name ':' prefix is removed for `elem` lookup.
    varList = getVarList d
    varDetails a =
      let vName = (G.unName . G._vdName $ a)
       in Inline $
            mkParam
              vName
              Nothing
              Nothing
              (if vName `elem` pathVars then ParamPath else ParamQuery)
              Nothing
              (getDefaultVar a)
              ( case G._vdType a of
                  G.TypeNamed _ na -> case G.unName na of
                    "Int" -> Just OpenApiInteger
                    "String" -> Just OpenApiString
                    "json" -> Just OpenApiObject
                    _ -> Nothing
                  G.TypeList _ _ -> Nothing
              )

getGQLQueryFromTrie :: EndpointMetadata GQLQueryWithText -> Text
getGQLQueryFromTrie = getGQLQueryText . _edQuery . _ceDefinition

mkParam :: Text -> Maybe Text -> Maybe Bool -> ParamLocation -> Maybe Bool -> Maybe Value -> Maybe OpenApiType -> Param
mkParam nameP desc req loc allowEmpty def varType =
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
        )

getDefaultVar :: G.VariableDefinition -> Maybe Value
getDefaultVar var = case G._vdDefaultValue var of
  Nothing -> Nothing
  Just va -> case va of
    G.VNull -> Nothing
    G.VInt n -> Just $ toJSON n
    G.VFloat sci -> Just $ toJSON sci
    G.VString txt -> Just $ toJSON txt
    G.VBoolean b -> Just $ toJSON b
    G.VEnum ev -> Just $ toJSON ev
    _ -> Nothing

getComment :: EndpointMetadata GQLQueryWithText -> Text
getComment d = comment
  where
    gql = getGQLQueryFromTrie d
    comment = case _ceComment d of
      (Just c) -> c <> "\n***\nThe GraphQl query for this endpoint is:\n``` graphql\n" <> gql <> "\n```"
      Nothing -> "***\nThe GraphQl query for this endpoint is:\n``` graphql\n" <> gql <> "\n```"

getURL :: EndpointMetadata GQLQueryWithText -> Text
getURL d =
  "/api/rest/"
    -- The url will be of the format <Endpoint>/:<Var1>/:<Var2> ... always, so we can
    -- split and take the first element (it should never fail)
    <> fst (T.breakOn "/" (TNE.unNonEmptyText . unEndpointUrl . _ceUrl $ d))
    <> foldl
      ( \b a -> b <> "/{" <> a <> "}"
      )
      ""
      (map T.tail $ lefts $ splitPath Left Right (_ceUrl d))

extractEndpointInfo :: EndpointMethod -> EndpointMetadata GQLQueryWithText -> EndpointData
extractEndpointInfo method d =
  let _edUrl = T.unpack . getURL $ d
      _edVarList = getVariableDefinitions d
      _edDescription = getComment d
      _edName = TNE.unNonEmptyText $ unEndpointName $ _ceName d
   in EndpointData
        { _edMethod = [unEndpointMethod method], -- NOTE: Methods are grouped with into matching endpoints - Name used for grouping.
          ..
        }

getEndpointsData :: SchemaCache -> [EndpointData]
getEndpointsData sc = map squashEndpointGroup endpointsGrouped
  where
    endpointTrie = scEndpoints sc
    methodMaps = leaves endpointTrie
    endpointsWithMethods = concatMap (\(m, s) -> map (m,) (S.toList s)) $ concatMap (M.toList . _unMultiMap) methodMaps
    endpointsWithInfo = map (uncurry extractEndpointInfo) endpointsWithMethods
    endpointsGrouped = LNE.groupBy (\a b -> _edName a == _edName b) endpointsWithInfo

squashEndpointGroup :: NonEmpty EndpointData -> EndpointData
squashEndpointGroup g = (LNE.head g) {_edMethod = concatMap _edMethod g}

serveJSON :: SchemaCache -> OpenApi
serveJSON sc = spec & components . schemas .~ defs
  where
    (defs, spec) = runDeclare (declareOpenApiSpec sc) mempty

declareOpenApiSpec :: SchemaCache -> Declare (Definitions Schema) OpenApi
declareOpenApiSpec sc = do
  let mkOperation :: EndpointData -> Operation
      mkOperation ed =
        mempty
          & description ?~ _edDescription ed
          & summary ?~ _edName ed
          & parameters .~ (Inline xHasuraAS : _edVarList ed)

      getOPName :: EndpointData -> Text -> Maybe Operation
      getOPName ed methodType =
        if methodType `elem` _edMethod ed
          then Just $ mkOperation ed
          else Nothing

      xHasuraAS :: Param
      xHasuraAS =
        mkParam
          "x-hasura-admin-secret"
          (Just "Your x-hasura-admin-secret will be used for authentication of the API request.")
          Nothing
          ParamHeader
          Nothing
          Nothing
          (Just OpenApiString)

      generatePathItem :: EndpointData -> PathItem
      generatePathItem ed =
        mempty
          & get .~ getOPName ed "GET"
          & post .~ getOPName ed "POST"
          & put .~ getOPName ed "PUT"
          & delete .~ getOPName ed "DELETE"
          & patch .~ getOPName ed "PATCH"

      endpointLst = getEndpointsData sc

      mkOpenAPISchema :: [EndpointData] -> InsOrdHashMap FilePath PathItem
      mkOpenAPISchema edLst = foldl (\hm ed -> MI.insertWith (<>) (_edUrl ed) (generatePathItem ed) hm) mempty edLst

      openAPIPaths = mkOpenAPISchema endpointLst

  return $
    mempty
      & paths .~ openAPIPaths
      & info . title .~ "Rest Endpoints"
      & info . description ?~ "These OpenAPI specifications are automatically generated by Hasura."
