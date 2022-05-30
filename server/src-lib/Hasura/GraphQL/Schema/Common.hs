{-# LANGUAGE TemplateHaskell #-}

module Hasura.GraphQL.Schema.Common
  ( SchemaOptions (..),
    SchemaContext (..),
    RemoteRelationshipParserBuilder (..),
    MonadBuildSchemaBase,
    retrieve,
    ignoreRemoteRelationship,
    AggSelectExp,
    AnnotatedField,
    AnnotatedFields,
    ConnectionFields,
    ConnectionSelectExp,
    AnnotatedActionField,
    AnnotatedActionFields,
    EdgeFields,
    Scenario (..),
    SelectArgs,
    SelectStreamArgs,
    SelectExp,
    StreamSelectExp,
    TablePerms,
    getTableRoles,
    askTableInfo,
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
    RemoteSchemaParser (..),
    mkEnumTypeName,
    addEnumSuffix,
    peelWithOrigin,
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
import Hasura.GraphQL.Namespace (NamespacedField)
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Parser.Internal.Parser qualified as P
import Hasura.GraphQL.Parser.Internal.TypeChecking qualified as P
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session (RoleName)
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------

-- | Aggregation of options required to build the schema.
data SchemaOptions = SchemaOptions
  { -- | how numbers should be represented in the ouput; this option does not
    -- influence the schema, but is bundled in the resulting IR
    soStringifyNum :: StringifyNumbers,
    -- | should boolean fields be collapsed to True when null is given?
    soDangerousBooleanCollapse :: Bool,
    -- | what kind of schema is being built (Hasura or Relay)
    soQueryType :: ET.GraphQLQueryType,
    -- | whether function permissions should be inferred
    soFunctionPermsContext :: FunctionPermissionsCtx,
    -- | whether remote schema permissions are enabled
    soRemoteSchemaPermsCtx :: RemoteSchemaPermsCtx,
    -- | 'True' when we should attempt to use experimental SQL optimization passes
    soOptimizePermissionFilters :: Bool
  }

-- | Aggregation of contextual information required to build the schema.
data SchemaContext = SchemaContext
  { -- | the set of all sources (TODO: remove this?)
    scSourceCache :: SourceCache,
    -- | how to process remote relationships
    scRemoteRelationshipParserBuilder :: RemoteRelationshipParserBuilder
  }

-- | The set of common constraints required to build the schema.
type MonadBuildSchemaBase r m n =
  ( MonadError QErr m,
    MonadReader r m,
    P.MonadSchema n m,
    Has SchemaOptions r,
    Has SchemaContext r,
    -- TODO: make all `Has x r` explicit fields of 'SchemaContext'
    Has RoleName r,
    Has MkTypename r,
    Has MkRootFieldName r,
    Has CustomizeRemoteFieldName r,
    Has NamingCase r
  )

-- | How a remote relationship field should be processed when building a
-- schema. Injecting this function from the top level avoids having to know how
-- to do top-level dispatch from deep within the schema code.
--
-- Note: the inner function type uses an existential qualifier: it is expected
-- that the given function will work for _any_ monad @m@ that has the relevant
-- constraints. This prevents us from passing a function that is specfic to the
-- monad in which the schema construction will run, but avoids having to
-- propagate type annotations to each call site.
newtype RemoteRelationshipParserBuilder
  = RemoteRelationshipParserBuilder
      ( forall lhsJoinField r n m.
        MonadBuildSchemaBase r m n =>
        RemoteFieldInfo lhsJoinField ->
        m (Maybe [P.FieldParser n (IR.RemoteRelationshipField IR.UnpreparedValue)])
      )

-- | A 'RemoteRelationshipParserBuilder' that ignores the field altogether, that can
-- be used in tests or to build a source or remote schema in isolation.
ignoreRemoteRelationship :: RemoteRelationshipParserBuilder
ignoreRemoteRelationship = RemoteRelationshipParserBuilder $ const $ pure Nothing

-- TODO: move this to Prelude?
retrieve ::
  (MonadReader r m, Has a r) =>
  (a -> b) ->
  m b
retrieve f = asks $ f . getter

-------------------------------------------------------------------------------

type SelectExp b = IR.AnnSimpleSelectG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type StreamSelectExp b = IR.AnnSimpleStreamSelectG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type AggSelectExp b = IR.AnnAggregateSelectG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type ConnectionSelectExp b = IR.ConnectionSelect b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type SelectArgs b = IR.SelectArgsG b (IR.UnpreparedValue b)

type SelectStreamArgs b = IR.SelectStreamArgsG b (IR.UnpreparedValue b)

type TablePerms b = IR.TablePermG b (IR.UnpreparedValue b)

type AnnotatedFields b = IR.AnnFieldsG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type AnnotatedField b = IR.AnnFieldG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type ConnectionFields b = IR.ConnectionFields b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type EdgeFields b = IR.EdgeFields b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type AnnotatedActionFields = IR.ActionFieldsG (IR.RemoteRelationshipField IR.UnpreparedValue)

type AnnotatedActionField = IR.ActionFieldG (IR.RemoteRelationshipField IR.UnpreparedValue)

-------------------------------------------------------------------------------

data RemoteSchemaParser n = RemoteSchemaParser
  { piQuery :: [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField IR.UnpreparedValue) RemoteSchemaVariable))],
    piMutation :: Maybe [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField IR.UnpreparedValue) RemoteSchemaVariable))],
    piSubscription :: Maybe [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField IR.UnpreparedValue) RemoteSchemaVariable))]
  }

getTableRoles :: BackendSourceInfo -> [RoleName]
getTableRoles bsi = AB.dispatchAnyBackend @Backend bsi go
  where
    go si = Map.keys . _tiRolePermInfoMap =<< Map.elems (_siTables si)

-- | Looks up table information for the given table name. This function
-- should never fail, since the schema cache construction process is
-- supposed to ensure all dependencies are resolved.
-- TODO: deduplicate this with `CacheRM`.
askTableInfo ::
  forall b m.
  (Backend b, MonadError QErr m) =>
  SourceInfo b ->
  TableName b ->
  m (TableInfo b)
askTableInfo SourceInfo {..} tableName =
  Map.lookup tableName _siTables
    `onNothing` throw500 ("askTableInfo: no info for table " <> dquote tableName <> " in source " <> dquote _siName)

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

textToName :: MonadError QErr m => Text -> m G.Name
textToName textName =
  G.mkName textName
    `onNothing` throw400
      ValidationFailed
      ( "cannot include " <> textName <<> " in the GraphQL schema because "
          <> " it is not a valid GraphQL identifier"
      )

partialSQLExpToUnpreparedValue :: PartialSQLExp b -> IR.UnpreparedValue b
partialSQLExpToUnpreparedValue (PSESessVar pftype var) = IR.UVSessionVar pftype var
partialSQLExpToUnpreparedValue PSESession = IR.UVSession
partialSQLExpToUnpreparedValue (PSESQLExp sqlExp) = IR.UVLiteral sqlExp

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
  Fields a
parsedSelectionsToFields mkTypename =
  OMap.toList
    >>> map (FieldName . G.unName *** P.handleTypename (mkTypename . G.unName))

numericAggOperators :: [G.Name]
numericAggOperators =
  [ G._sum,
    G._avg,
    G._stddev,
    G._stddev_samp,
    G._stddev_pop,
    G._variance,
    G._var_samp,
    G._var_pop
  ]

comparisonAggOperators :: [G.Name]
comparisonAggOperators = [$$(G.litName "max"), $$(G.litName "min")]

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

-- | Builds the type name for referenced enum tables.
mkEnumTypeName :: forall b m r. (Backend b, MonadReader r m, Has MkTypename r, MonadError QErr m) => EnumReference b -> m G.Name
mkEnumTypeName (EnumReference enumTableName _ enumTableCustomName) = do
  enumTableGQLName <- tableGraphQLName @b enumTableName `onLeft` throwError
  addEnumSuffix enumTableGQLName enumTableCustomName

addEnumSuffix :: (MonadReader r m, Has MkTypename r) => G.Name -> Maybe G.Name -> m G.Name
addEnumSuffix enumTableGQLName enumTableCustomName = P.mkTypename $ (fromMaybe enumTableGQLName enumTableCustomName) <> G.__enum

-- TODO: figure out what the purpose of this method is.
peelWithOrigin :: P.MonadParse m => P.Parser 'P.Both m a -> P.Parser 'P.Both m (IR.ValueWithOrigin a)
peelWithOrigin parser =
  parser
    { P.pParser = \case
        P.GraphQLValue (G.VVariable var@P.Variable {vInfo, vValue}) -> do
          -- Check types c.f. 5.8.5 of the June 2018 GraphQL spec
          P.typeCheck False (P.toGraphQLType $ P.pType parser) var
          IR.ValueWithOrigin vInfo <$> P.pParser parser (absurd <$> vValue)
        value -> IR.ValueNoOrigin <$> P.pParser parser value
    }
