-- | Tools for generating fields for Apollo federation
module Hasura.GraphQL.ApolloFederation
  ( -- * Field Parser generators
    apolloRootFields,
    ApolloFederationParserFunction (..),
    convertToApolloFedParserFunc,
    getApolloFederationStatus,
    generateSDLWithAllTypes,
    generateSDL,
  )
where

import Control.Lens ((??))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KMap
import Data.Aeson.Ordered qualified as JO
import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text qualified as T
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.Root
import Hasura.RQL.IR.Value (UnpreparedValue, ValueWithOrigin (ValueNoOrigin))
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Schema.Options (StringifyNumbers)
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types
import Hasura.Table.Cache
import Language.GraphQL.Draft.Printer qualified as Printer
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as Builder

-- | Internal parser function for entities field
data ApolloFederationParserFunction n = ApolloFederationParserFunction
  { aafuGetRootField :: ApolloFederationAnyType -> n (QueryRootField UnpreparedValue)
  }

-- | Haskell representation of _Any scalar
data ApolloFederationAnyType = ApolloFederationAnyType
  { afTypename :: G.Name,
    afPKValues :: J.Object
  }
  deriving stock (Show)

-- | Parser for _Any scalar
anyParser :: P.Parser origin 'Both Parse ApolloFederationAnyType
anyParser =
  jsonScalar Name.__Any (Just "Scalar _Any") `bind` \val -> do
    let typenameKey = K.fromText "__typename"
    case val of
      J.Object obj -> case KMap.lookup typenameKey obj of
        Just (J.String txt) -> case G.mkName txt of
          Just tName ->
            pure
              $ ApolloFederationAnyType
                { afTypename = tName,
                  afPKValues = KMap.delete typenameKey obj
                }
          Nothing -> P.parseError $ toErrorMessage $ txt <> " is not a valid graphql name"
        Nothing -> P.parseError $ toErrorMessage "__typename key not found"
        _ -> P.parseError $ toErrorMessage "__typename can only be a string value"
      _ -> P.parseError $ toErrorMessage "representations is expecting a list of objects only"

convertToApolloFedParserFunc ::
  (MonadParse n, Backend b) =>
  SourceInfo b ->
  TableInfo b ->
  SelPermInfo b ->
  StringifyNumbers ->
  Maybe NamingCase ->
  NESeq (ColumnInfo b) ->
  Parser 'Output n (AnnotatedFields b) ->
  Parser 'Output n (ApolloFederationParserFunction n)
convertToApolloFedParserFunc sInfo tInfo selectPermissions stringifyNumbers tCase pKeys =
  fmap (modifyApolloFedParserFunc sInfo tInfo selectPermissions stringifyNumbers tCase pKeys)

modifyApolloFedParserFunc ::
  (MonadParse n, Backend b) =>
  SourceInfo b ->
  TableInfo b ->
  SelPermInfo b ->
  StringifyNumbers ->
  Maybe NamingCase ->
  NESeq (ColumnInfo b) ->
  AnnotatedFields b ->
  ApolloFederationParserFunction n
modifyApolloFedParserFunc
  SourceInfo {..}
  TableInfo {..}
  selectPermissions
  stringifyNumbers
  tCase
  primaryKeys
  annField = ApolloFederationParserFunction $ \ApolloFederationAnyType {..} -> do
    allConstraints <-
      for primaryKeys \columnInfo -> do
        let colName = G.unName $ ciName columnInfo
            cvType = ciType columnInfo
            redactionExp = fromMaybe IR.NoRedaction $ getRedactionExprForColumn selectPermissions (ciColumn columnInfo)
        cvValue <- case KMap.lookup (K.fromText colName) afPKValues of
          Nothing -> P.parseError . toErrorMessage $ "cannot find " <> colName <> " in _Any type"
          Just va -> liftQErr $ flip runReaderT _siConfiguration $ parseScalarValueColumnType (ciType columnInfo) va
        pure
          $ IR.BoolField
          . IR.AVColumn columnInfo redactionExp
          . pure
          . IR.AEQ IR.NonNullableComparison
          . IR.mkParameter
          $ ValueNoOrigin
          $ ColumnValue {..}
    let whereExpr = Just $ IR.BoolAnd $ toList allConstraints
        sourceName = _siName
        sourceConfig = _siConfiguration
        tableName = _tciName _tiCoreInfo
        queryDBRoot =
          IR.QDBR
            $ IR.QDBSingleRow
            $ IR.AnnSelectG
              { IR._asnFields = annField,
                IR._asnFrom = IR.FromTable tableName,
                IR._asnPerm = tableSelPerm,
                IR._asnArgs = IR.noSelectArgs {IR._saWhere = whereExpr},
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = tCase
              }
    pure
      $ IR.RFDB sourceName
      $ AB.mkAnyBackend
      $ IR.SourceConfigWith sourceConfig Nothing
      $ queryDBRoot
    where
      tableSelPerm = tablePermissionsInfo selectPermissions
      liftQErr = either (P.parseError . toErrorMessage . qeError) pure . runExcept

-------------------------------------------------------------------------------
-- Related to @service@ field

-- main function

-- | Creates @_service@ @FieldParser@ using the schema introspection.
--   This will allow us to process the following query:
--
--   > query {
--   >   _service {
--   >     sdl
--   >   }
--   > }
mkServiceField ::
  FieldParser P.Parse (G.SchemaIntrospection -> QueryRootField UnpreparedValue)
mkServiceField = serviceFieldParser
  where
    sdlField = JO.String . generateSDL <$ P.selection_ Name._sdl (Just "SDL representation of schema") P.string
    serviceParser = P.nonNullableParser $ P.selectionSet Name.__Service Nothing [sdlField]
    serviceFieldParser =
      P.subselection_ Name.__service Nothing serviceParser `bindField` \selSet -> do
        let partialValue = InsOrdHashMap.map (\ps -> handleTypename (\tName _ -> JO.toOrdered tName) ps) (InsOrdHashMap.mapKeys G.unName selSet)
        pure \schemaIntrospection -> RFRaw . JO.fromOrderedHashMap $ (partialValue ?? schemaIntrospection)

apolloRootFields ::
  ApolloFederationStatus ->
  [(G.Name, Parser 'Output P.Parse (ApolloFederationParserFunction P.Parse))] ->
  [FieldParser P.Parse (G.SchemaIntrospection -> QueryRootField UnpreparedValue)]
apolloRootFields apolloFederationStatus apolloFedTableParsers =
  let -- generate the `_service` field parser
      serviceField = mkServiceField

      -- generate the `_entities` field parser
      entityField = const <$> mkEntityUnionFieldParser apolloFedTableParsers
   in -- we would want to expose these fields inorder to support apollo federation
      -- refer https://www.apollographql.com/docs/federation/federation-spec
      -- `serviceField` is essential to connect hasura to gateway, `entityField`
      -- is essential only if we have types that has @key directive
      if
        | isApolloFederationEnabled apolloFederationStatus && not (null apolloFedTableParsers) ->
            [serviceField, entityField]
        | isApolloFederationEnabled apolloFederationStatus ->
            [serviceField]
        | otherwise -> []

-- helpers

-- | Check if the Apollo Federation feature is enabled or not. If the user has explicitly set the Apollo Federation
--   status, then we use that else we fallback to the experimental feature flag
getApolloFederationStatus :: HashSet ExperimentalFeature -> Maybe ApolloFederationStatus -> ApolloFederationStatus
getApolloFederationStatus experimentalFeatures Nothing =
  bool ApolloFederationDisabled ApolloFederationEnabled (EFApolloFederation `elem` experimentalFeatures)
getApolloFederationStatus _ (Just apolloFederationStatus) = apolloFederationStatus

data GenerateSDLType
  = -- | Preserves schema types (GraphQL types prefixed with __) in the sdl generated
    AllTypes
  | -- | Removes schema types (GraphQL types prefixed with __) in the sdl generated
    RemoveSchemaTypes
  deriving (Eq)

generateSDLFromIntrospection :: GenerateSDLType -> G.SchemaIntrospection -> Text
generateSDLFromIntrospection genSdlType (G.SchemaIntrospection sIntro) = sdl
  where
    -- NOTE:  add this to the sdl to support apollo v2 directive
    _supportV2 :: Text
    _supportV2 = "\n\nextend schema\n@link(url: \"https://specs.apollo.dev/federation/v2.0\",\nimport: [\"@key\", \"@shareable\"])"

    schemaFilterFn = bool id filterTypeDefinition (genSdlType == RemoveSchemaTypes)

    -- first we filter out the type definitions which are not relevent such as
    -- schema fields and types (starts with `__`)
    typeDefns = map (G.TypeSystemDefinitionType . schemaFilterFn . bimap (const ()) id) (HashMap.elems sIntro)

    -- next we get the root operation type definitions
    rootOpTypeDefns =
      mapMaybe
        ( \(fieldName, operationType) ->
            HashMap.lookup fieldName sIntro
              $> G.RootOperationTypeDefinition operationType fieldName
        )
        [ (Name._query_root, G.OperationTypeQuery),
          (Name._mutation_root, G.OperationTypeMutation),
          (Name._subscription_root, G.OperationTypeSubscription)
        ]

    -- finally we gather everything, run the printer and generate full sdl in `Text`
    sdl = Builder.run $ Printer.schemaDocument getSchemaDocument

    getSchemaDocument :: G.SchemaDocument
    getSchemaDocument =
      G.SchemaDocument
        $ G.TypeSystemDefinitionSchema (G.SchemaDefinition Nothing rootOpTypeDefns)
        : typeDefns

-- | Filter out schema components from sdl which are not required by apollo federation
filterTypeDefinition :: G.TypeDefinition possibleTypes G.InputValueDefinition -> G.TypeDefinition possibleTypes G.InputValueDefinition
filterTypeDefinition = \case
  G.TypeDefinitionObject (G.ObjectTypeDefinition a b c d e) ->
    -- We are skipping the schema types here
    G.TypeDefinitionObject
      $ G.ObjectTypeDefinition a b c d (filter (not . T.isPrefixOf "__" . G.unName . G._fldName) e)
  typeDef -> typeDef

generateSDLWithAllTypes :: G.SchemaIntrospection -> Text
generateSDLWithAllTypes = generateSDLFromIntrospection AllTypes

generateSDL :: G.SchemaIntrospection -> Text
generateSDL = generateSDLFromIntrospection RemoveSchemaTypes

-------------------------------------------------------------------------------
-- Related to @_entities@ field

-- main function

-- | Creates @_entities@ @FieldParser@ using `Parser`s for Entity union, schema
--   introspection and a list of all query `FieldParser`.
--   This will allow us to process the following query:
--
--   > query ($representations: [_Any!]!) {
--   >   _entities(representations: $representations) {
--   >     ... on SomeType {
--   >       foo
--   >       bar
--   >     }
--   >   }
--   > }
mkEntityUnionFieldParser ::
  [(G.Name, Parser 'Output Parse (ApolloFederationParserFunction Parse))] ->
  FieldParser P.Parse (QueryRootField UnpreparedValue)
mkEntityUnionFieldParser apolloFedTableParsers =
  let entityParserMap = HashMap.fromList apolloFedTableParsers

      -- the Union `Entities`
      bodyParser = P.selectionSetUnion Name.__Entity (Just "A union of all types that use the @key directive") entityParserMap

      -- name of the field
      name = Name.__entities

      -- description of the field
      description = Just "query _Entity union"

      representationParser =
        field Name._representations Nothing $ list $ anyParser

      entityParser =
        subselection name description representationParser bodyParser
          `bindField` \(parsedArgs, parsedBody) -> do
            rootFields <-
              for
                parsedArgs
                ( \anyArg ->
                    case HashMap.lookup (afTypename anyArg) parsedBody of
                      Nothing -> (P.parseError . toErrorMessage) $ G.unName (afTypename anyArg) <> " is not found in selection set or apollo federation is not enabled for the type"
                      Just aafus -> (aafuGetRootField aafus) anyArg
                )
            pure $ concatQueryRootFields rootFields
   in entityParser

-- | concatenates multiple fields
concatQueryRootFields :: [QueryRootField UnpreparedValue] -> QueryRootField UnpreparedValue
concatQueryRootFields = RFMulti
