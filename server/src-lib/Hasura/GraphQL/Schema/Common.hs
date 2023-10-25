{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.GraphQL.Schema.Common
  ( SchemaContext (..),
    SchemaSampledFeatureFlags (..),
    sampleFeatureFlags,
    WithSchemaSampledFeatureFlags,
    withSchemaSampledFeatureFlags,
    SchemaKind (..),
    RemoteRelationshipParserBuilder (..),
    NodeInterfaceParserBuilder (..),
    MonadBuildSchemaBase,
    retrieve,
    SchemaT (..),
    MonadBuildSourceSchema,
    MonadBuildRemoteSchema,
    MonadBuildActionSchema,
    runSourceSchema,
    runRemoteSchema,
    runActionSchema,
    ignoreRemoteRelationship,
    isHasuraSchema,
    AggSelectExp,
    AnnotatedField,
    AnnotatedFields,
    ConnectionFields,
    ConnectionSelectExp,
    AnnotatedActionField,
    AnnotatedActionFields,
    AnnotatedNestedObjectSelect,
    AnnotatedNestedArraySelect,
    EdgeFields,
    Scenario (..),
    SelectArgs,
    SelectStreamArgs,
    SelectExp,
    StreamSelectExp,
    TablePerms,
    getTableRoles,
    getLogicalModelRoles,
    askScalarTypeParsingContext,
    askTableInfo,
    askLogicalModelInfo,
    askNativeQueryInfo,
    comparisonAggOperators,
    mapField,
    mkDescriptionWith,
    numericAggOperators,
    optionalFieldParser,
    parsedSelectionsToFields,
    partialSQLExpToUnpreparedValue,
    getRedactionExprForColumn,
    getRedactionExprForComputedField,
    requiredFieldParser,
    takeValidNativeQueries,
    takeValidStoredProcedures,
    takeValidFunctions,
    takeValidTables,
    textToName,
    textToGQLIdentifier,
    RemoteSchemaParser (..),
    mkEnumTypeName,
    addEnumSuffix,
    peelWithOrigin,
    getIntrospectionResult,
    tablePermissionsInfo,
  )
where

import Control.Monad.Trans.Control
import Data.Either (isRight)
import Data.Has
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List (uncons)
import Data.Text qualified as T
import Data.Text.Casing (GQLNameIdentifier)
import Data.Text.Casing qualified as C
import Data.Text.Extended
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.GraphQL.Namespace (NamespacedField)
import Hasura.GraphQL.Parser.Internal.TypeChecking qualified as P
import Hasura.GraphQL.Schema.Node
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Typename
import Hasura.LogicalModel.Cache (LogicalModelInfo (_lmiPermissions))
import Hasura.LogicalModel.Types (LogicalModelName)
import Hasura.NativeQuery.Cache (NativeQueryCache, NativeQueryInfo (..))
import Hasura.NativeQuery.Types (NativeQueryName)
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField.Name (ComputedFieldName)
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.RQL.Types.Schema.Options (SchemaOptions)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.SchemaCache hiding (askTableInfo)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RemoteSchema.SchemaCache.Types
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Init.FeatureFlag qualified as FF
import Hasura.StoredProcedure.Cache (StoredProcedureCache)
import Hasura.Table.Cache (SelPermInfo (..))
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------

-- | Aggregation of contextual information required to build the schema.
data SchemaContext = SchemaContext
  { -- | the kind of schema being built
    scSchemaKind :: SchemaKind,
    -- | how to process remote relationships
    scRemoteRelationshipParserBuilder :: RemoteRelationshipParserBuilder,
    -- | the role for which the schema is being built
    scRole :: RoleName,
    scSampledFeatureFlags :: SchemaSampledFeatureFlags
  }

-- | We want to be able to probe feature flags in the schema parsers, but we also
-- want to be able to run schema actions without requiring IO, in part because we
-- want to be able to run them in tests and in part because we want some assurance
-- that the schema we parse doesn't change without our knowledge, as that precludes
-- safely caching schema introspection.
newtype SchemaSampledFeatureFlags = SchemaSampledFeatureFlags [(FF.FeatureFlag, Bool)]
  deriving (Eq, Show)

sampleFeatureFlags :: FF.CheckFeatureFlag -> IO SchemaSampledFeatureFlags
sampleFeatureFlags checkFeatureFlag = do
  let ffs = map fst $ FF.listKnownFeatureFlags checkFeatureFlag
  SchemaSampledFeatureFlags <$> mapM (\ff -> (ff,) <$> FF.runCheckFeatureFlag checkFeatureFlag ff) ffs

-- | Monad transformer that lets you use the sampled feature flags from a reader environment.
-- This is necessary because we want 'ReaderT r m` to be transparent wrt. 'HasFeatureFlagChecker m'.
newtype WithSchemaSampledFeatureFlags m a = WithSchemaSampledFeatureFlags {unWithSchemaSampledFeatureFlags :: ReaderT SchemaSampledFeatureFlags m a}
  deriving (Functor, Applicative, Monad, MonadIO)

deriving instance (MonadBase IO m) => MonadBase IO (WithSchemaSampledFeatureFlags m)

deriving instance (MonadBaseControl IO m, MonadBase IO m) => MonadBaseControl IO (WithSchemaSampledFeatureFlags m)

instance MonadTrans WithSchemaSampledFeatureFlags where
  lift = WithSchemaSampledFeatureFlags . lift

instance (MonadResolveSource m) => MonadResolveSource (WithSchemaSampledFeatureFlags m) where
  getPGSourceResolver = lift getPGSourceResolver
  getMSSQLSourceResolver = lift getMSSQLSourceResolver

withSchemaSampledFeatureFlags :: SchemaSampledFeatureFlags -> WithSchemaSampledFeatureFlags m a -> m a
withSchemaSampledFeatureFlags ffs = flip runReaderT ffs . unWithSchemaSampledFeatureFlags

instance (Monad m) => FF.HasFeatureFlagChecker (WithSchemaSampledFeatureFlags m) where
  checkFlag ff = do
    flags <- WithSchemaSampledFeatureFlags $ ask
    pure $ sampledCheckFlag flags ff

sampledCheckFlag :: SchemaSampledFeatureFlags -> FF.FeatureFlag -> Bool
sampledCheckFlag (SchemaSampledFeatureFlags flags) ff = fromMaybe False (lookup ff flags)

-- | The kind of schema we're building, and its associated options.
data SchemaKind
  = HasuraSchema
  | RelaySchema NodeInterfaceParserBuilder

isHasuraSchema :: SchemaKind -> Bool
isHasuraSchema = \case
  HasuraSchema -> True
  RelaySchema _ -> False

-- | The set of common constraints required to build the schema.
type MonadBuildSchemaBase m n =
  ( MonadError QErr m,
    P.MonadMemoize m,
    P.MonadParse n
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
        (MonadBuildSchemaBase m n) =>
        RemoteFieldInfo lhsJoinField ->
        SchemaT r m (Maybe [P.FieldParser n (IR.RemoteRelationshipField IR.UnpreparedValue)])
      )

-- | A 'RemoteRelationshipParserBuilder' that ignores the field altogether, that can
-- be used in tests or to build a source or remote schema in isolation.
ignoreRemoteRelationship :: RemoteRelationshipParserBuilder
ignoreRemoteRelationship = RemoteRelationshipParserBuilder $ const $ pure Nothing

-- | How to build the 'Relay' node.
--
-- Similarly to what we do for remote relationships, we pass in the context the
-- builder function required to build the 'Node' interface, in order to avoid
-- the cross-sources cycles it creates otherwise.
newtype NodeInterfaceParserBuilder = NodeInterfaceParserBuilder
  { runNodeBuilder ::
      ( forall m n.
        (MonadBuildSchemaBase m n) =>
        SchemaContext ->
        SchemaOptions ->
        m (P.Parser 'P.Output n NodeMap)
      )
  }

-- TODO: move this to Prelude?
retrieve ::
  (MonadReader r m, Has a r) =>
  (a -> b) ->
  m b
retrieve f = asks $ f . getter

-------------------------------------------------------------------------------

{- Note [SchemaT and stacking]

The schema is explicitly built in `SchemaT`, rather than in an arbitrary monad
`m` that happens to have the desired properties (`MonadReader`, `MonadMemoize`,
`MonadError`, and so on). The main reason why we do this is that we want to
avoid a specific performance issue that arises out of two specific constraints:

  - we want to build each part of the schema (such as sources and remote
    schemas) with its own dedicated minimal reader context (i.e. not using a
    shared reader context that is the union of all the information required);
  - we want to be able to process remote-relationships, which means "altering"
    the reader context when jumping from one "part" of the schema to another.

What that means, in practice, is that we have to call `runReaderT` (or an
equivalent) every time we build a part of the schema (at the root level or as
part of a remote relationship) so that the part we build has access to its
context. When processing a remote relationship, the calling code is *already* in
a monad stack that contains a `ReaderT`, since we were processing a given part
of the schema. If we directly call `runReaderT` to process the RHS of the remote
relationship, we implicitly make it so that the monad stack of the LHS is the
base underneath the `ReaderT` of the RHS; in other terms, we stack another
reader on top of the existing monad stack.

As the schema is built in a "depth-first" way, in a complicated schema with a
lot of remote relationships we would end up with several readers stacked upon
one another. A manually run benchmark showed that this could significantly
impact performance in complicated schemas. We do now have a benchmark set to
replicate this specific case (see the "deep_schema" benchmark set for more
information).

To prevent this stacking, we need to be able to "bring back" the result of the
`runReaderT` back into the calling monad, rather than defaulting to having the
calling monad be the base of the reader. The simplest way of doing this is to
enforce that we are always building the schema in a monad stack that has the
reader on top of some arbitrary *shared* base. This gives us the guarantee that
the LHS of any remote relationship, the calling context for `runReaderT`, is
itself a `ReaderT` on top og that known shared base, meaning that after a call
to `runReaderT` on another part of the schema, we can always go back to the
calling monad with a simple `lift`, as demonstrated in
'remoteRelationshipField'.
-}

-- | The monad in which the schema is built.
--
-- The implementation of 'SchemaT' is intended to be opaque: running a
-- computation in 'SchemaT' is intended to be done via calls to
-- 'runSourceSchema' and 'runRemoteSchema', which also enforce what the @r@
-- parameter should be in each case.
--
-- The reason why we want to enforce that the schema is built in a reader on top
-- of an arbitrary base monad is for performance: see Note [SchemaT and
-- stacking] for more information.
--
-- In the future, we might monomorphize this further to make `MemoizeT` explicit.
newtype SchemaT r m a = SchemaT {runSchemaT :: ReaderT r m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, P.MonadMemoize, MonadTrans, MonadError e)

instance (Has SchemaContext r, Monad m) => FF.HasFeatureFlagChecker (SchemaT r m) where
  checkFlag ff = do
    flags <- asks (scSampledFeatureFlags . getter)
    pure $ sampledCheckFlag flags ff

type MonadBuildSourceSchema b r m n =
  ( MonadBuildSchemaBase m n,
    Has SchemaContext r,
    Has SchemaOptions r,
    Has (SourceInfo b) r
  )

-- | Runs a schema-building computation with all the context required to build a source.
runSourceSchema ::
  forall b m a.
  SchemaContext ->
  SchemaOptions ->
  SourceInfo b ->
  SchemaT
    ( SchemaContext,
      SchemaOptions,
      SourceInfo b
    )
    m
    a ->
  m a
runSourceSchema context options sourceInfo (SchemaT action) = runReaderT action (context, options, sourceInfo)

type MonadBuildRemoteSchema r m n =
  ( MonadBuildSchemaBase m n,
    Has SchemaContext r,
    Has Options.RemoteNullForwardingPolicy r,
    Has CustomizeRemoteFieldName r,
    Has MkTypename r
  )

-- | Runs a schema-building computation with all the context required to build a remote schema.
runRemoteSchema ::
  SchemaContext ->
  Options.RemoteNullForwardingPolicy ->
  SchemaT
    ( SchemaContext,
      Options.RemoteNullForwardingPolicy,
      MkTypename,
      CustomizeRemoteFieldName
    )
    m
    a ->
  m a
runRemoteSchema context nullForwarding (SchemaT action) = runReaderT action (context, nullForwarding, mempty, mempty)

type MonadBuildActionSchema r m n =
  ( MonadBuildSchemaBase m n,
    Has SchemaContext r,
    Has SchemaOptions r
  )

-- | Runs a schema-building computation with all the context required to build actions.
runActionSchema ::
  SchemaContext ->
  SchemaOptions ->
  SchemaT
    ( SchemaContext,
      SchemaOptions
    )
    m
    a ->
  m a
runActionSchema context options (SchemaT action) = runReaderT action (context, options)

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

type AnnotatedNestedObjectSelect b = IR.AnnNestedObjectSelectG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

type AnnotatedNestedArraySelect b = IR.AnnNestedArraySelectG b (IR.RemoteRelationshipField IR.UnpreparedValue) (IR.UnpreparedValue b)

-------------------------------------------------------------------------------

data RemoteSchemaParser n = RemoteSchemaParser
  { piQuery :: [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField IR.UnpreparedValue) RemoteSchemaVariable))],
    piMutation :: Maybe [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField IR.UnpreparedValue) RemoteSchemaVariable))],
    piSubscription :: Maybe [P.FieldParser n (NamespacedField (IR.RemoteSchemaRootField (IR.RemoteRelationshipField IR.UnpreparedValue) RemoteSchemaVariable))]
  }

getTableRoles :: BackendSourceInfo -> [RoleName]
getTableRoles bsi = AB.dispatchAnyBackend @Backend bsi go
  where
    go si = HashMap.keys . _tiRolePermInfoMap =<< HashMap.elems (_siTables si)

getLogicalModelRoles :: BackendSourceInfo -> [RoleName]
getLogicalModelRoles bsi = AB.dispatchAnyBackend @Backend bsi go
  where
    go si =
      let namedLogicalModelRoles = HashMap.keys . _lmiPermissions =<< HashMap.elems (_siLogicalModels si)
          inlineLogicalModelRoles = HashMap.keys . _lmiPermissions . _nqiReturns =<< HashMap.elems (_siNativeQueries si)
       in namedLogicalModelRoles <> inlineLogicalModelRoles

askScalarTypeParsingContext ::
  forall b r m.
  (MonadReader r m, Has (SourceInfo b) r, Has (ScalarTypeParsingContext b) (SourceConfig b)) =>
  m (ScalarTypeParsingContext b)
askScalarTypeParsingContext = asks (getter . _siConfiguration @b . getter)

-- | Looks up table information for the given table name. This function
-- should never fail, since the schema cache construction process is
-- supposed to ensure all dependencies are resolved.
-- TODO: deduplicate this with `CacheRM`.
askTableInfo ::
  forall b r m.
  (Backend b, MonadError QErr m, MonadReader r m, Has (SourceInfo b) r) =>
  TableName b ->
  m (TableInfo b)
askTableInfo tableName = do
  SourceInfo {..} <- asks getter
  HashMap.lookup tableName _siTables
    `onNothing` throw500 ("askTableInfo: no info for table " <> dquote tableName <> " in source " <> dquote _siName)

-- | Looks up logical model information for the given logical model name. This function
-- should never fail, since the schema cache construction process is
-- supposed to ensure all dependencies are resolved.
-- TODO: deduplicate this with `CacheRM`.
askLogicalModelInfo ::
  forall b r m.
  (MonadError QErr m, MonadReader r m, Has (SourceInfo b) r) =>
  LogicalModelName ->
  m (LogicalModelInfo b)
askLogicalModelInfo logicalModelName = do
  SourceInfo {..} <- asks getter
  HashMap.lookup logicalModelName _siLogicalModels
    `onNothing` throw500 ("askLogicalModelInfo: no info for logical model " <> dquote logicalModelName <> " in source " <> dquote _siName)

-- | Looks up native query information for the given native query name. This function
-- should never fail, since the schema cache construction process is
-- supposed to ensure all dependencies are resolved.
-- TODO: deduplicate this with `CacheRM`.
askNativeQueryInfo ::
  forall b r m.
  (MonadError QErr m, MonadReader r m, Has (SourceInfo b) r) =>
  NativeQueryName ->
  m (NativeQueryInfo b)
askNativeQueryInfo nativeQueryName = do
  SourceInfo {..} <- asks getter
  HashMap.lookup nativeQueryName _siNativeQueries
    `onNothing` throw500 ("askNativeQueryInfo: no info for native query " <> dquote nativeQueryName <> " in source " <> dquote _siName)

-- | Whether the request is sent with `x-hasura-use-backend-only-permissions` set to `true`.
data Scenario = Backend | Frontend deriving (Enum, Show, Eq)

textToName :: (MonadError QErr m) => Text -> m G.Name
textToName textName =
  G.mkName textName
    `onNothing` throw400
      ValidationFailed
      ( "cannot include "
          <> textName
          <<> " in the GraphQL schema because "
          <> " it is not a valid GraphQL identifier"
      )

textToGQLIdentifier :: (MonadError QErr m) => Text -> m GQLNameIdentifier
textToGQLIdentifier textName = do
  let gqlIdents = do
        (pref, suffs) <- uncons (C.fromSnake textName)
        prefName <- G.mkName pref
        suffNames <- traverse G.mkNameSuffix suffs
        pure $ C.fromAutogeneratedTuple (prefName, suffNames)
  gqlIdents
    `onNothing` throw400
      ValidationFailed
      ( "cannot include "
          <> textName
          <<> " in the GraphQL schema because "
          <> " it is not a valid GraphQL identifier"
      )

partialSQLExpToUnpreparedValue :: PartialSQLExp b -> IR.UnpreparedValue b
partialSQLExpToUnpreparedValue (PSESessVar pftype var) = IR.UVSessionVar pftype var
partialSQLExpToUnpreparedValue PSESession = IR.UVSession
partialSQLExpToUnpreparedValue (PSESQLExp sqlExp) = IR.UVLiteral sqlExp

getRedactionExprForColumn :: (Backend b) => SelPermInfo b -> Column b -> Maybe (IR.AnnRedactionExpUnpreparedValue b)
getRedactionExprForColumn selectPermissions columnName =
  let redactionExp = HashMap.lookup columnName (spiCols selectPermissions)
   in fmap partialSQLExpToUnpreparedValue <$> redactionExp

getRedactionExprForComputedField :: (Backend b) => SelPermInfo b -> ComputedFieldName -> Maybe (IR.AnnRedactionExpUnpreparedValue b)
getRedactionExprForComputedField selectPermissions cfName =
  let redactionExp = HashMap.lookup cfName (spiComputedFields selectPermissions)
   in fmap partialSQLExpToUnpreparedValue <$> redactionExp

mapField ::
  (Functor m) =>
  P.InputFieldsParser m (Maybe a) ->
  (a -> b) ->
  P.InputFieldsParser m (Maybe b)
mapField fp f = fmap (fmap f) fp

parsedSelectionsToFields ::
  -- | how to handle @__typename@ fields
  (Text -> a) ->
  InsOrdHashMap.InsOrdHashMap G.Name (P.ParsedSelection a) ->
  Fields a
parsedSelectionsToFields mkTypenameFromText =
  InsOrdHashMap.toList
    >>> map (FieldName . G.unName *** P.handleTypename (mkTypenameFromText . G.unName))

numericAggOperators :: [C.GQLNameIdentifier]
numericAggOperators =
  [ C.fromAutogeneratedName $$(G.litName "sum"),
    C.fromAutogeneratedName $$(G.litName "avg"),
    C.fromAutogeneratedName $$(G.litName "stddev"),
    C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["stddev", "samp"]),
    C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["stddev", "pop"]),
    C.fromAutogeneratedName $$(G.litName "variance"),
    C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["var", "samp"]),
    C.fromAutogeneratedTuple $$(G.litGQLIdentifier ["var", "pop"])
  ]

comparisonAggOperators :: [C.GQLNameIdentifier]
comparisonAggOperators =
  [ C.fromAutogeneratedName $$(G.litName "max"),
    C.fromAutogeneratedName $$(G.litName "min")
  ]

mkDescriptionWith :: Maybe Postgres.PGDescription -> Text -> G.Description
mkDescriptionWith descM defaultTxt = G.Description $ case descM of
  Nothing -> defaultTxt
  Just (Postgres.PGDescription descTxt) -> T.unlines [descTxt, "\n", defaultTxt]

-- TODO why do we do these validations at this point? What does it mean to track
--      a function but not add it to the schema...?
--      Auke:
--        I believe the intention is simply to allow the console to do postgres data management
--      Karthikeyan: Yes, this is correct. We allowed this pre PDV but somehow
--        got removed in PDV. OTOH, I’m not sure how prevalent this feature
--        actually is
takeValidTables :: forall b. (Backend b) => TableCache b -> TableCache b
takeValidTables = HashMap.filterWithKey graphQLTableFilter
  where
    graphQLTableFilter tableName tableInfo =
      -- either the table name should be GraphQL compliant
      -- or it should have a GraphQL custom name set with it
      isRight (tableGraphQLName @b tableName)
        || isJust (_tcCustomName $ _tciCustomConfig $ _tiCoreInfo tableInfo)

-- TODO and what about graphql-compliant function names here too?
takeValidFunctions :: forall b. FunctionCache b -> FunctionCache b
takeValidFunctions = HashMap.filter functionFilter
  where
    functionFilter = not . isSystemDefined . _fiSystemDefined

-- | @TODO: Currently we do no validation on native queries in schema. Should we?
takeValidNativeQueries :: forall b. NativeQueryCache b -> NativeQueryCache b
takeValidNativeQueries = id

-- | @TODO: Currently we do no validation on stored procedures in schema. Should we?
takeValidStoredProcedures :: forall b. StoredProcedureCache b -> StoredProcedureCache b
takeValidStoredProcedures = id

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
mkEnumTypeName :: forall b r m. (Backend b, MonadError QErr m, Has (SourceInfo b) r) => TableName b -> Maybe G.Name -> SchemaT r m G.Name
mkEnumTypeName enumTableName enumTableCustomName = do
  customization <- retrieve $ _siCustomization @b
  enumTableGQLName <- getTableIdentifier @b enumTableName `onLeft` throwError
  pure $ addEnumSuffix customization enumTableGQLName enumTableCustomName

addEnumSuffix :: ResolvedSourceCustomization -> GQLNameIdentifier -> Maybe G.Name -> G.Name
addEnumSuffix customization enumTableGQLName enumTableCustomName =
  runMkTypename (_rscTypeNames customization)
    $ applyTypeNameCaseIdentifier (_rscNamingConvention customization)
    $ mkEnumTableTypeName enumTableGQLName enumTableCustomName

-- TODO: figure out what the purpose of this method is.
peelWithOrigin :: (P.MonadParse m) => P.Parser 'P.Both m a -> P.Parser 'P.Both m (IR.ValueWithOrigin a)
peelWithOrigin parser =
  parser
    { P.pParser = \case
        P.GraphQLValue (G.VVariable var@P.Variable {vInfo, vValue}) -> do
          -- Check types c.f. 5.8.5 of the June 2018 GraphQL spec
          P.typeCheck False (P.toGraphQLType $ P.pType parser) var
          fmap (IR.ValueWithOrigin vInfo)
            $ P.pParser parser
            $ case vValue of
              -- TODO: why is faking a null value here semantically correct? RE: GraphQL spec June 2018, section 2.9.5
              Nothing -> P.GraphQLValue G.VNull
              Just val -> absurd <$> val
        value -> IR.ValueNoOrigin <$> P.pParser parser value
    }

getIntrospectionResult :: Options.RemoteSchemaPermissions -> RoleName -> RemoteSchemaCtxG remoteFieldInfo -> Maybe IntrospectionResult
getIntrospectionResult remoteSchemaPermsCtx role remoteSchemaContext =
  if
    | -- admin doesn't have a custom annotated introspection, defaulting to the original one
      role == adminRoleName ->
        pure $ _rscIntroOriginal remoteSchemaContext
    | -- if permissions are disabled, the role map will be empty, defaulting to the original one
      remoteSchemaPermsCtx == Options.DisableRemoteSchemaPermissions ->
        pure $ _rscIntroOriginal remoteSchemaContext
    | -- otherwise, look the role up in the map; if we find nothing, then the role doesn't have access
      otherwise ->
        HashMap.lookup role (_rscPermissions remoteSchemaContext)

tablePermissionsInfo :: (Backend b) => SelPermInfo b -> TablePerms b
tablePermissionsInfo selectPermissions =
  IR.TablePerm
    { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
      IR._tpLimit = spiLimit selectPermissions
    }
