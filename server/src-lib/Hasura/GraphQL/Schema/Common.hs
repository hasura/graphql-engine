{-# LANGUAGE TemplateHaskell #-}

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
    QueryContext (..),
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
    getIndirectDependencies,
    purgeDependencies,
    dropRemoteSchemaRemoteRelationshipInMetadata,
    mkEnumTypeName,
    addEnumSuffix,
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
import Hasura.Prelude
import Hasura.RQL.IR.Action qualified as IR
import Hasura.RQL.IR.RemoteSchema qualified as IR
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.Types hiding (askTableInfo)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session (RoleName)
import Language.GraphQL.Draft.Syntax as G

-- | the set of common constraints required to build the schema
type MonadBuildSchemaBase r m n =
  ( MonadError QErr m,
    MonadReader r m,
    P.MonadSchema n m,
    Has RoleName r,
    Has SourceCache r,
    Has QueryContext r,
    Has MkTypename r,
    Has MkRootFieldName r,
    Has CustomizeRemoteFieldName r,
    Has RemoteSchemaMap r
  )

type SelectExp b = IR.AnnSimpleSelectG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type StreamSelectExp b = IR.AnnSimpleStreamSelectG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AggSelectExp b = IR.AnnAggregateSelectG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type ConnectionSelectExp b = IR.ConnectionSelect b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type SelectArgs b = IR.SelectArgsG b (P.UnpreparedValue b)

type SelectStreamArgs b = IR.SelectStreamArgsG b (P.UnpreparedValue b)

type TablePerms b = IR.TablePermG b (P.UnpreparedValue b)

type AnnotatedFields b = IR.AnnFieldsG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AnnotatedField b = IR.AnnFieldG b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type ConnectionFields b = IR.ConnectionFields b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type EdgeFields b = IR.EdgeFields b (IR.RemoteRelationshipField P.UnpreparedValue) (P.UnpreparedValue b)

type AnnotatedActionFields = IR.ActionFieldsG (IR.RemoteRelationshipField P.UnpreparedValue)

type AnnotatedActionField = IR.ActionFieldG (IR.RemoteRelationshipField P.UnpreparedValue)

data RemoteSchemaParser n = RemoteSchemaParser
  { piQuery :: [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField P.UnpreparedValue) RemoteSchemaVariable))],
    piMutation :: Maybe [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField P.UnpreparedValue) RemoteSchemaVariable))],
    piSubscription :: Maybe [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField P.UnpreparedValue) RemoteSchemaVariable))]
  }

data QueryContext = QueryContext
  { qcStringifyNum :: StringifyNumbers,
    -- | should boolean fields be collapsed to True when null is given?
    qcDangerousBooleanCollapse :: Bool,
    qcQueryType :: ET.GraphQLQueryType,
    qcFunctionPermsContext :: FunctionPermissionsCtx,
    qcRemoteSchemaPermsCtx :: RemoteSchemaPermsCtx,
    -- | 'True' when we should attempt to use experimental SQL optimization passes
    qcOptimizePermissionFilters :: Bool
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
  forall b r m.
  (Backend b, MonadError QErr m, MonadReader r m, Has SourceCache r) =>
  SourceName ->
  TableName b ->
  m (TableInfo b)
askTableInfo sourceName tableName = do
  tableInfo <- asks $ getTableInfo . getter
  -- This should never fail, since the schema cache construction process is
  -- supposed to ensure that all dependencies are resolved.
  tableInfo `onNothing` throw500 ("askTableInfo: no info for table " <> dquote tableName <> " in source " <> dquote sourceName)
  where
    getTableInfo :: SourceCache -> Maybe (TableInfo b)
    getTableInfo = Map.lookup tableName <=< unsafeSourceTables <=< Map.lookup sourceName

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

-- | Builds the type name for referenced enum tables.
mkEnumTypeName :: forall b m r. (Backend b, MonadReader r m, Has MkTypename r, MonadError QErr m) => EnumReference b -> m G.Name
mkEnumTypeName (EnumReference enumTableName _ enumTableCustomName) = do
  enumTableGQLName <- tableGraphQLName @b enumTableName `onLeft` throwError
  addEnumSuffix enumTableGQLName enumTableCustomName

addEnumSuffix :: (MonadReader r m, Has MkTypename r) => G.Name -> Maybe G.Name -> m G.Name
addEnumSuffix enumTableGQLName enumTableCustomName = P.mkTypename $ (fromMaybe enumTableGQLName enumTableCustomName) <> G.__enum

-- | Return the indirect dependencies on a source.
-- We return a [SchemaObjId] instead of [SourceObjId], because the latter has no
-- reference to the source. Due to which we won't be able to drop the dependencies of
-- a different source.
--
-- For eg:
-- Say we have two sources: "source1" and "source2".
-- "source1" has a remote relationship with "source2". So when "source2" is dropped,
-- the remote relationship from "source1" also has to be dropped. If we were to only
-- pass the 'SourceObjId', we wouldn't have know which source that pertains to. Hence
-- we'll not be able to drop the remote relationship. That's why we return the
-- [SchemaObjId] so that, we can drop the 'SourceObjId' from the correct source.
getIndirectDependencies ::
  forall m.
  (QErrM m, CacheRM m) =>
  SourceName ->
  m [SchemaObjId]
getIndirectDependencies sourceName = do
  schemaCache <- askSchemaCache
  -- (Direct + Indirect dependencies)
  let allDependecies = getDependentObjs schemaCache (SOSource sourceName)
  pure $ filter isIndirectDep allDependecies
  where
    isIndirectDep :: SchemaObjId -> Bool
    isIndirectDep (SOSourceObj sn _)
      -- If the dependency is in another source, then it is an indirect dependency
      | sn /= sourceName = True
      | otherwise = False
    -- If a relationship in a remote schema depends on this source, then we have to
    -- remove the relationship from remote schema.
    isIndirectDep (SORemoteSchemaRemoteRelationship {}) = True
    -- Ignore non source dependencies
    isIndirectDep _ = False

purgeDependencies ::
  MonadError QErr m =>
  [SchemaObjId] ->
  WriterT MetadataModifier m ()
purgeDependencies deps =
  for_ deps \case
    SOSourceObj sourceName objectID -> do
      AB.dispatchAnyBackend @BackendMetadata objectID $ purgeDependentObject sourceName >=> tell
    SORemoteSchemaRemoteRelationship remoteSchemaName typeName relationshipName -> do
      tell $ dropRemoteSchemaRemoteRelationshipInMetadata remoteSchemaName typeName relationshipName
    _ ->
      -- Ignore non-source dependencies
      pure ()

dropRemoteSchemaRemoteRelationshipInMetadata :: RemoteSchemaName -> G.Name -> RelName -> MetadataModifier
dropRemoteSchemaRemoteRelationshipInMetadata remoteSchemaName typeName relationshipName =
  MetadataModifier $
    metaRemoteSchemas
      . ix remoteSchemaName
      . rsmRemoteRelationships
      . ix typeName
      . rstrsRelationships
      %~ OMap.delete relationshipName
