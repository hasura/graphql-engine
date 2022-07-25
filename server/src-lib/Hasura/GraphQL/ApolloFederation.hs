-- | Tools for generating fields for Apollo federation
module Hasura.GraphQL.ApolloFederation
  ( -- * Field Parser generators
    mkEntityUnionFieldParser,
    mkServiceField,
    apolloRootFields,
    ApolloFederationParserFunction (..),
    convertToApolloFedParserFunc,
  )
where

import Control.Lens ((??))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KMap
import Data.Aeson.Ordered qualified as JO
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.NamingCase
import Hasura.GraphQL.Schema.Options (StringifyNumbers)
import Hasura.GraphQL.Schema.Parser
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.Root
import Hasura.RQL.IR.Select
import Hasura.RQL.IR.Value (UnpreparedValue, ValueWithOrigin (ValueNoOrigin))
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types
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
            pure $
              ApolloFederationAnyType
                { afTypename = tName,
                  afPKValues = KMap.delete typenameKey obj
                }
          Nothing -> P.parseError $ toErrorMessage $ txt <> " is not a valid graphql name"
        Nothing -> P.parseError $ toErrorMessage "__typename key not found"
        _ -> P.parseError $ toErrorMessage "__typename can only be a string value"
      _ -> P.parseError $ toErrorMessage "representations is expecting a list of objects only"

convertToApolloFedParserFunc ::
  (Monad n, MonadParse n, Backend b) =>
  SourceInfo b ->
  TableInfo b ->
  TablePermG b (UnpreparedValue b) ->
  StringifyNumbers ->
  Maybe NamingCase ->
  NESeq (ColumnInfo b) ->
  Parser 'Output n (AnnotatedFields b) ->
  Parser 'Output n (ApolloFederationParserFunction n)
convertToApolloFedParserFunc sInfo tInfo selPerm stringifyNumbers tCase pKeys =
  fmap (modifyApolloFedParserFunc sInfo tInfo selPerm stringifyNumbers tCase pKeys)

modifyApolloFedParserFunc ::
  (MonadParse n, Backend b) =>
  SourceInfo b ->
  TableInfo b ->
  TablePermG b (UnpreparedValue b) ->
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
        cvValue <- case KMap.lookup (K.fromText colName) afPKValues of
          Nothing -> P.parseError . toErrorMessage $ "cannot find " <> colName <> " in _Any type"
          Just va -> liftQErr $ parseScalarValueColumnType (ciType columnInfo) va
        pure $
          IR.BoolField . IR.AVColumn columnInfo . pure . IR.AEQ True . IR.mkParameter $
            ValueNoOrigin $ ColumnValue {..}
    let whereExpr = Just $ IR.BoolAnd $ toList allConstraints
        sourceName = _siName
        sourceConfig = _siConfiguration
        tableName = _tciName _tiCoreInfo
        queryDBRoot =
          IR.QDBR $
            IR.QDBSingleRow $
              IR.AnnSelectG
                { IR._asnFields = annField,
                  IR._asnFrom = IR.FromTable tableName,
                  IR._asnPerm = selectPermissions,
                  IR._asnArgs = IR.noSelectArgs {IR._saWhere = whereExpr},
                  IR._asnStrfyNum = stringifyNumbers,
                  IR._asnNamingConvention = tCase
                }
    pure $
      IR.RFDB sourceName $
        AB.mkAnyBackend $
          IR.SourceConfigWith sourceConfig Nothing $
            queryDBRoot
    where
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
        let partialValue = OMap.map (\ps -> handleTypename (\tName _ -> JO.toOrdered tName) ps) (OMap.mapKeys G.unName selSet)
        pure \schemaIntrospection -> RFRaw . JO.fromOrderedHashMap $ (partialValue ?? schemaIntrospection)

apolloRootFields ::
  Set.HashSet ExperimentalFeature ->
  [(G.Name, Parser 'Output P.Parse (ApolloFederationParserFunction P.Parse))] ->
  [FieldParser P.Parse (G.SchemaIntrospection -> QueryRootField UnpreparedValue)]
apolloRootFields expFeatures apolloFedTableParsers =
  let -- generate the `_service` field parser
      serviceField = mkServiceField

      -- generate the `_entities` field parser
      entityField = const <$> mkEntityUnionFieldParser apolloFedTableParsers
   in -- we would want to expose these fields inorder to support apollo federation
      -- refer https://www.apollographql.com/docs/federation/federation-spec
      -- `serviceField` is essential to connect hasura to gateway, `entityField`
      -- is essential only if we have types that has @key directive
      if
          | EFApolloFederation `elem` expFeatures && not (null apolloFedTableParsers) ->
            [serviceField, entityField]
          | EFApolloFederation `elem` expFeatures ->
            [serviceField]
          | otherwise -> []

-- helpers

-- | Generate sdl from the schema introspection
generateSDL :: G.SchemaIntrospection -> Text
generateSDL (G.SchemaIntrospection sIntro) = sdl
  where
    -- NOTE:  add this to the sdl to support apollo v2 directive
    _supportV2 :: Text
    _supportV2 = "\n\nextend schema\n@link(url: \"https://specs.apollo.dev/federation/v2.0\",\nimport: [\"@key\", \"@shareable\"])"

    -- first we filter out the type definitions which are not relevent such as
    -- schema fields and types (starts with `__`)
    typeDefns = mapMaybe filterAndWrapTypeSystemDefinition (Map.elems sIntro)

    -- next we get the root operation type definitions
    rootOpTypeDefns =
      mapMaybe
        ( \(fieldName, operationType) ->
            Map.lookup fieldName sIntro
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
      G.SchemaDocument $
        G.TypeSystemDefinitionSchema (G.SchemaDefinition Nothing (rootOpTypeDefns)) : typeDefns

-- | Filter out schema components from sdl which are not required by apollo federation and
-- wraps it in `TypeSystemDefinition`
filterAndWrapTypeSystemDefinition :: G.TypeDefinition [G.Name] G.InputValueDefinition -> Maybe G.TypeSystemDefinition
filterAndWrapTypeSystemDefinition = \case
  G.TypeDefinitionScalar (G.ScalarTypeDefinition {}) -> Nothing
  G.TypeDefinitionInterface (G.InterfaceTypeDefinition a b c d _) ->
    Just $ G.TypeSystemDefinitionType (G.TypeDefinitionInterface (G.InterfaceTypeDefinition a b c d ()))
  G.TypeDefinitionObject (G.ObjectTypeDefinition a b c d e) ->
    -- We are skipping the schema types here
    Just . G.TypeSystemDefinitionType . G.TypeDefinitionObject $
      G.ObjectTypeDefinition a b c d (filter (not . T.isPrefixOf "__" . G.unName . G._fldName) e)
  G.TypeDefinitionUnion defn -> Just $ G.TypeSystemDefinitionType (G.TypeDefinitionUnion defn)
  G.TypeDefinitionEnum defn -> Just $ G.TypeSystemDefinitionType (G.TypeDefinitionEnum defn)
  G.TypeDefinitionInputObject defn -> Just $ G.TypeSystemDefinitionType (G.TypeDefinitionInputObject defn)

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
  let entityParserMap = Map.fromList apolloFedTableParsers

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
                    case Map.lookup (afTypename anyArg) parsedBody of
                      Nothing -> (P.parseError . toErrorMessage) $ G.unName (afTypename anyArg) <> " is not found in selection set or apollo federation is not enabled for the type"
                      Just aafus -> (aafuGetRootField aafus) anyArg
                )
            pure $ concatQueryRootFields rootFields
   in entityParser

-- | concatenates multiple fields
concatQueryRootFields :: [QueryRootField UnpreparedValue] -> QueryRootField UnpreparedValue
concatQueryRootFields = RFMulti
