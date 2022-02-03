module Hasura.GraphQL.Schema.Common
  ( MonadBuildSchemaBase,
    AggSelectExp,
    AnnotatedField,
    AnnotatedFields,
    ConnectionFields,
    ConnectionSelectExp,
    AnnotatedActionField,
    AnnotatedActionFields,
    EdgeFields,
    QueryContext (QueryContext, qcDangerousBooleanCollapse, qcFunctionPermsContext, qcQueryType, qcRemoteRelationshipContext, qcStringifyNum, qcOptimizePermissionFilters),
    RemoteRelationshipQueryContext (RemoteRelationshipQueryContext, _rrscParsedIntrospection),
    SelectArgs,
    SelectExp,
    TablePerms,
    comparisonAggOperators,
    currentNodeIdVersion,
    mapField,
    mkDescriptionWith,
    nodeIdVersionInt,
    numericAggOperators,
    optionalFieldParser,
    parsedSelectionsToFields,
    partialSQLExpToUnpreparedValue,
    requiredFieldParser,
    takeValidFunctions,
    takeValidTables,
    textToName,
  )
where

import Data.Aeson qualified as J
import Data.Either (isRight)
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types qualified as PG
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Types qualified as ET (GraphQLQueryType)
import Hasura.GraphQL.Parser qualified as P
import Hasura.Prelude
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types
import Language.GraphQL.Draft.Syntax as G

-- | the set of common constraints required to build the schema
type MonadBuildSchemaBase r m n =
  ( MonadError QErr m,
    P.MonadSchema n m,
    P.MonadTableInfo r m,
    P.MonadRole r m,
    Has QueryContext r,
    Has MkTypename r,
    Has MkRootFieldName r,
    Has CustomizeRemoteFieldName r
  )

type SelectExp b = IR.AnnSimpleSelectG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AggSelectExp b = IR.AnnAggregateSelectG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type ConnectionSelectExp b = IR.ConnectionSelect b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type SelectArgs b = IR.SelectArgsG b (P.UnpreparedValue b)

type TablePerms b = IR.TablePermG b (P.UnpreparedValue b)

type AnnotatedFields b = IR.AnnFieldsG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AnnotatedField b = IR.AnnFieldG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type ConnectionFields b = IR.ConnectionFields b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type EdgeFields b = IR.EdgeFields b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AnnotatedActionFields b = IR.ActionFieldsG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AnnotatedActionField b = IR.ActionFieldG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

data RemoteRelationshipQueryContext = RemoteRelationshipQueryContext
  { _rrscIntrospectionResultOriginal :: !IntrospectionResult,
    _rrscParsedIntrospection :: !ParsedIntrospection,
    _rrscRemoteSchemaCustomizer :: !RemoteSchemaCustomizer
  }

data QueryContext = QueryContext
  { qcStringifyNum :: Bool,
    -- | should boolean fields be collapsed to True when null is given?
    qcDangerousBooleanCollapse :: Bool,
    qcQueryType :: ET.GraphQLQueryType,
    qcRemoteRelationshipContext :: HashMap RemoteSchemaName RemoteRelationshipQueryContext,
    qcFunctionPermsContext :: FunctionPermissionsCtx,
    -- | 'True' when we should attempt to use experimental SQL optimization passes
    qcOptimizePermissionFilters :: Bool
  }

textToName :: MonadError QErr m => Text -> m G.Name
textToName textName =
  G.mkName textName
    `onNothing` throw400
      ValidationFailed
      ( "cannot include " <> textName <<> " in the GraphQL schema because "
          <> " it is not a valid GraphQL identifier"
      )

partialSQLExpToUnpreparedValue :: PartialSQLExp b -> P.UnpreparedValue b
partialSQLExpToUnpreparedValue (PSESessVar pftype var) = P.UVSessionVar pftype var
partialSQLExpToUnpreparedValue PSESession = P.UVSession
partialSQLExpToUnpreparedValue (PSESQLExp sqlExp) = P.UVLiteral sqlExp

mapField ::
  Functor m =>
  P.InputFieldsParser m (Maybe a) ->
  (a -> b) ->
  P.InputFieldsParser m (Maybe b)
mapField fp f = fmap (fmap f) fp

parsedSelectionsToFields ::
  -- | how to handle @__typename@ fields
  (Text -> a) ->
  OMap.InsOrdHashMap G.Name (P.ParsedSelection a) ->
  IR.Fields a
parsedSelectionsToFields mkTypename =
  OMap.toList
    >>> map (FieldName . G.unName *** P.handleTypename (mkTypename . G.unName))

numericAggOperators :: [G.Name]
numericAggOperators =
  [ $$(G.litName "sum"),
    $$(G.litName "avg"),
    $$(G.litName "stddev"),
    $$(G.litName "stddev_samp"),
    $$(G.litName "stddev_pop"),
    $$(G.litName "variance"),
    $$(G.litName "var_samp"),
    $$(G.litName "var_pop")
  ]

comparisonAggOperators :: [G.Name]
comparisonAggOperators = [$$(litName "max"), $$(litName "min")]

data NodeIdVersion
  = NIVersion1
  deriving (Show, Eq)

nodeIdVersionInt :: NodeIdVersion -> Int
nodeIdVersionInt NIVersion1 = 1

currentNodeIdVersion :: NodeIdVersion
currentNodeIdVersion = NIVersion1

instance J.FromJSON NodeIdVersion where
  parseJSON v = do
    versionInt :: Int <- J.parseJSON v
    case versionInt of
      1 -> pure NIVersion1
      _ -> fail $ "expecting version 1 for node id, but got " <> show versionInt

mkDescriptionWith :: Maybe PG.PGDescription -> Text -> G.Description
mkDescriptionWith descM defaultTxt = G.Description $ case descM of
  Nothing -> defaultTxt
  Just (PG.PGDescription descTxt) -> T.unlines [descTxt, "\n", defaultTxt]

-- TODO why do we do these validations at this point? What does it mean to track
--      a function but not add it to the schema...?
--      Auke:
--        I believe the intention is simply to allow the console to do postgres data management
--      Karthikeyan: Yes, this is correct. We allowed this pre PDV but somehow
--        got removed in PDV. OTOH, I’m not sure how prevalent this feature
--        actually is
takeValidTables :: forall b. Backend b => TableCache b -> TableCache b
takeValidTables = Map.filterWithKey graphQLTableFilter . Map.filter tableFilter
  where
    tableFilter = not . isSystemDefined . _tciSystemDefined . _tiCoreInfo
    graphQLTableFilter tableName tableInfo =
      -- either the table name should be GraphQL compliant
      -- or it should have a GraphQL custom name set with it
      isRight (tableGraphQLName @b tableName)
        || isJust (_tcCustomName $ _tciCustomConfig $ _tiCoreInfo tableInfo)

-- TODO and what about graphql-compliant function names here too?
takeValidFunctions :: forall b. FunctionCache b -> FunctionCache b
takeValidFunctions = Map.filter functionFilter
  where
    functionFilter = not . isSystemDefined . _fiSystemDefined

-- root field builder helpers

requiredFieldParser ::
  (Functor n, Functor m) =>
  (a -> b) ->
  m (P.FieldParser n a) ->
  m (Maybe (P.FieldParser n b))
requiredFieldParser f = fmap $ Just . fmap f

optionalFieldParser ::
  (Functor n, Functor m) =>
  (a -> b) ->
  m (Maybe (P.FieldParser n a)) ->
  m (Maybe (P.FieldParser n b))
optionalFieldParser = fmap . fmap . fmap
