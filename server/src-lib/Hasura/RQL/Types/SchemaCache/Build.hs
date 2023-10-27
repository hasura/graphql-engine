{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types and functions used in the process of building the schema cache from metadata information
-- stored in the @hdb_catalog@ schema in Postgres.
module Hasura.RQL.Types.SchemaCache.Build
  ( MetadataDependency (..),
    recordInconsistencies,
    recordInconsistencyM,
    recordInconsistenciesM,
    recordDependencies,
    recordDependenciesM,
    withRecordInconsistency,
    withRecordInconsistencyM,
    withRecordInconsistencies,
    CacheRWM (..),
    buildSchemaCacheWithOptions,
    BuildReason (..),
    CacheInvalidations (..),
    ValidateNewSchemaCache,
    ValidateNewSchemaCacheResult (..),
    MetadataM (..),
    MetadataT (..),
    runMetadataT,
    buildSchemaCacheWithInvalidations,
    buildSchemaCache,
    tryBuildSchemaCache,
    tryBuildSchemaCacheWithModifiers,
    tryBuildSchemaCacheAndWarnOnFailingObjects,
    buildSchemaCacheFor,
    throwOnInconsistencies,
    withNewInconsistentObjsCheck,
    getInconsistentQueryCollections,
    StoredIntrospection (..),
    StoredIntrospectionItem (..),
    CollectItem (..),
  )
where

import Control.Arrow.Extended
import Control.Monad.Morph
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, genericParseJSON, genericToEncoding, genericToJSON)
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.Multi qualified as MultiMap
import Data.List qualified as L
import Data.List.Extended qualified as L
import Data.Sequence qualified as Seq
import Data.Text.Extended
import Data.Text.NonEmpty (unNonEmptyText)
import Data.Trie qualified as Trie
import Database.PG.Query qualified as PG
import Hasura.Backends.DataConnector.Adapter.Types (DataConnectorName)
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.GraphQL.Analyse
import Hasura.GraphQL.Transport.HTTP.Protocol
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.DDL.Warnings
import Hasura.RQL.Types.Allowlist (NormalizedQuery, unNormalizedQuery)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Endpoint
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.QueryCollection
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Session
import Hasura.RemoteSchema.Metadata (RemoteSchemaName)
import Hasura.Server.Init.FeatureFlag (HasFeatureFlagChecker)
import Hasura.Server.Types (MonadGetPolicies (..))
import Hasura.Services.Network
import Hasura.Tracing (TraceT)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G

-- * Inconsistencies

recordInconsistencies ::
  (ArrowWriter (Seq CollectItem) arr, Functor f, Foldable f) => ((Maybe Value, f MetadataObject), Text) `arr` ()
recordInconsistencies = proc ((val, mo), reason) ->
  tellA -< Seq.fromList $ toList $ fmap (CollectInconsistentMetadata . InconsistentObject reason val) mo

recordInconsistencyM ::
  (MonadWriter (Seq CollectItem) m) => Maybe Value -> MetadataObject -> Text -> m ()
recordInconsistencyM val mo reason = recordInconsistenciesM' [(val, mo)] reason

recordInconsistenciesM ::
  (MonadWriter (Seq CollectItem) m) => [MetadataObject] -> Text -> m ()
recordInconsistenciesM metadataObjects reason = recordInconsistenciesM' ((Nothing,) <$> metadataObjects) reason

recordInconsistenciesM' ::
  (MonadWriter (Seq CollectItem) m) => [(Maybe Value, MetadataObject)] -> Text -> m ()
recordInconsistenciesM' metadataObjects reason =
  tell $ Seq.fromList $ map (CollectInconsistentMetadata . uncurry (InconsistentObject reason)) metadataObjects

-- * Dependencies

data MetadataDependency
  = MetadataDependency
      -- | for error reporting on missing dependencies
      MetadataObject
      SchemaObjId
      SchemaDependency
  deriving (Eq, Show)

recordDependencies ::
  (ArrowWriter (Seq CollectItem) arr) =>
  (MetadataObject, SchemaObjId, Seq SchemaDependency) `arr` ()
recordDependencies = proc (metadataObject, schemaObjectId, dependencies) ->
  tellA -< CollectMetadataDependency . MetadataDependency metadataObject schemaObjectId <$> dependencies

recordDependenciesM ::
  (MonadWriter (Seq CollectItem) m) =>
  MetadataObject ->
  SchemaObjId ->
  Seq SchemaDependency ->
  m ()
recordDependenciesM metadataObject schemaObjectId dependencies = do
  tell $ CollectMetadataDependency . MetadataDependency metadataObject schemaObjectId <$> dependencies

-- * Helpers

-- | Monadic version of 'withRecordInconsistency'
withRecordInconsistencyM ::
  (MonadWriter (Seq CollectItem) m) =>
  MetadataObject ->
  ExceptT QErr m a ->
  m (Maybe a)
withRecordInconsistencyM metadataObject f = do
  result <- runExceptT f
  case result of
    Left err -> do
      case qeInternal err of
        Just (ExtraExtensions exts) ->
          -- the QErr type contains an optional qeInternal :: Maybe QErrExtra field, which either stores an error coming
          -- from an action webhook (ExtraExtensions) or an internal error thrown somewhere within graphql-engine.
          --
          -- if we do have an error here, it should be an internal error and hence never be of the former case:
          recordInconsistencyM (Just (toJSON exts)) metadataObject "withRecordInconsistency: unexpected ExtraExtensions"
        Just (ExtraInternal internal) ->
          recordInconsistencyM (Just (toJSON internal)) metadataObject (qeError err)
        Nothing ->
          recordInconsistencyM Nothing metadataObject (qeError err)
      return Nothing
    Right v -> return $ Just v

recordInconsistenciesWith ::
  (ArrowChoice arr, ArrowWriter (Seq CollectItem) arr) =>
  ((ArrowWriter (Seq CollectItem) arr) => ((Maybe Value, mo), Text) `arr` ()) ->
  ErrorA QErr arr (e, s) a ->
  arr (e, (mo, s)) (Maybe a)
recordInconsistenciesWith recordInconsistency' f = proc (e, (metadataObject, s)) -> do
  result <- runErrorA f -< (e, s)
  case result of
    Left err -> do
      case qeInternal err of
        Just (ExtraExtensions exts) ->
          -- the QErr type contains an optional qeInternal :: Maybe QErrExtra field, which either stores an error coming
          -- from an action webhook (ExtraExtensions) or an internal error thrown somewhere within graphql-engine.
          --
          -- if we do have an error here, it should be an internal error and hence never be of the former case:
          recordInconsistency' -< ((Just (toJSON exts), metadataObject), "withRecordInconsistency: unexpected ExtraExtensions")
        Just (ExtraInternal internal) ->
          recordInconsistency' -< ((Just (toJSON internal), metadataObject), qeError err)
        Nothing ->
          recordInconsistency' -< ((Nothing, metadataObject), qeError err)
      returnA -< Nothing
    Right v -> returnA -< Just v
{-# INLINEABLE recordInconsistenciesWith #-}

-- | Record any errors resulting from a computation as inconsistencies
withRecordInconsistency ::
  (ArrowChoice arr, ArrowWriter (Seq CollectItem) arr) =>
  ErrorA QErr arr (e, s) a ->
  arr (e, (MetadataObject, s)) (Maybe a)
withRecordInconsistency err = proc (e, (mo, s)) ->
  recordInconsistenciesWith recordInconsistencies err -< (e, ((Identity mo), s))
{-# INLINEABLE withRecordInconsistency #-}

withRecordInconsistencies ::
  (ArrowChoice arr, ArrowWriter (Seq CollectItem) arr) =>
  ErrorA QErr arr (e, s) a ->
  arr (e, ([MetadataObject], s)) (Maybe a)
withRecordInconsistencies = recordInconsistenciesWith recordInconsistencies
{-# INLINEABLE withRecordInconsistencies #-}

-- ----------------------------------------------------------------------------
-- operations for triggering a schema cache rebuild

class (CacheRM m) => CacheRWM m where
  tryBuildSchemaCacheWithOptions :: BuildReason -> CacheInvalidations -> Metadata -> Maybe MetadataResourceVersion -> ValidateNewSchemaCache a -> m a
  setMetadataResourceVersionInSchemaCache :: MetadataResourceVersion -> m ()

buildSchemaCacheWithOptions :: (CacheRWM m) => BuildReason -> CacheInvalidations -> Metadata -> Maybe MetadataResourceVersion -> m ()
buildSchemaCacheWithOptions buildReason cacheInvalidation metadata metadataResourceVersion =
  tryBuildSchemaCacheWithOptions buildReason cacheInvalidation metadata metadataResourceVersion (\_ _ -> (KeepNewSchemaCache, ()))

data BuildReason
  = -- | The build was triggered by an update this instance made to the catalog (in the
    -- currently-active transaction), so information in Postgres that needs to be kept in sync with
    -- the catalog (i.e. table event triggers in @hdb_catalog@ schema) should be updated.
    CatalogUpdate (Maybe (HashSet SourceName))
  | -- | The build was triggered by a notification that some other currently-running Hasura instance
    -- updated the catalog. Since that instance already updated table event triggers in @hdb_catalog@,
    -- this build should be read-only.
    CatalogSync
  deriving (Eq, Show)

data CacheInvalidations = CacheInvalidations
  { -- | Force reloading of all database information, including information not technically stored in
    -- metadata (currently just enum values). Set by the @reload_metadata@ API.
    ciMetadata :: Bool,
    -- | Force refetching of the given remote schemas, even if their definition has not changed. Set
    -- by the @reload_remote_schema@ API.
    ciRemoteSchemas :: HashSet RemoteSchemaName,
    -- | Force re-establishing connections of the given data sources, even if their configuration has not changed. Set
    -- by the @pg_reload_source@ API.
    ciSources :: HashSet SourceName,
    -- | Force re-fetching of `DataConnectorInfo` from the named data connectors.
    ciDataConnectors :: HashSet DataConnectorName
  }
  deriving stock (Generic)

instance FromJSON CacheInvalidations where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON CacheInvalidations where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance Semigroup CacheInvalidations where
  CacheInvalidations a1 b1 c1 d1 <> CacheInvalidations a2 b2 c2 d2 =
    CacheInvalidations (a1 || a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance Monoid CacheInvalidations where
  mempty = CacheInvalidations False mempty mempty mempty

-- | Function that validates the new schema cache (usually involves checking for any metadata inconsistencies)
-- and can decide whether or not to keep or discard the new schema cache ('ValidateNewSchemaCacheResult'). It
-- can also return some arbitrary extra information that will be returned from 'tryBuildSchemaCacheWithOptions'.
--
-- First parameter is the old schema cache, the second is the new schema cache.
type ValidateNewSchemaCache a = SchemaCache -> SchemaCache -> (ValidateNewSchemaCacheResult, a)

data ValidateNewSchemaCacheResult
  = KeepNewSchemaCache
  | DiscardNewSchemaCache
  deriving stock (Eq, Show, Ord)

instance (CacheRWM m) => CacheRWM (ReaderT r m) where
  tryBuildSchemaCacheWithOptions a b c d e = lift $ tryBuildSchemaCacheWithOptions a b c d e
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache

instance (CacheRWM m) => CacheRWM (StateT s m) where
  tryBuildSchemaCacheWithOptions a b c d e = lift $ tryBuildSchemaCacheWithOptions a b c d e
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache

instance (CacheRWM m) => CacheRWM (TraceT m) where
  tryBuildSchemaCacheWithOptions a b c d e = lift $ tryBuildSchemaCacheWithOptions a b c d e
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache

instance (CacheRWM m) => CacheRWM (PG.TxET QErr m) where
  tryBuildSchemaCacheWithOptions a b c d e = lift $ tryBuildSchemaCacheWithOptions a b c d e
  setMetadataResourceVersionInSchemaCache = lift . setMetadataResourceVersionInSchemaCache

newtype MetadataT m a = MetadataT {unMetadataT :: StateT Metadata m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadIO,
      MonadReader r,
      MonadError e,
      MonadTx,
      TableCoreInfoRM b,
      CacheRM,
      CacheRWM,
      MFunctor,
      Tracing.MonadTraceContext,
      Tracing.MonadTrace,
      MonadBase b,
      MonadBaseControl b,
      ProvidesNetwork,
      HasFeatureFlagChecker
    )
  deriving anyclass (MonadQueryTags)

instance (Monad m) => MetadataM (MetadataT m) where
  getMetadata = MetadataT get
  putMetadata = MetadataT . put

instance (UserInfoM m) => UserInfoM (MetadataT m) where
  askUserInfo = lift askUserInfo

instance (MonadGetPolicies m) => MonadGetPolicies (MetadataT m) where
  runGetApiTimeLimit = lift runGetApiTimeLimit
  runGetPrometheusMetricsGranularity = lift runGetPrometheusMetricsGranularity
  runGetModelInfoLogStatus = lift $ runGetModelInfoLogStatus

-- | @runMetadataT@ puts a stateful metadata in scope. @MetadataDefaults@ is
-- provided so that it can be considered from the --metadataDefaults arguments.
runMetadataT :: Metadata -> MetadataDefaults -> MetadataT m a -> m (a, Metadata)
runMetadataT metadata defaults (MetadataT m) =
  runStateT m (metadata `overrideMetadataDefaults` defaults)

buildSchemaCacheWithInvalidations :: (MetadataM m, CacheRWM m) => CacheInvalidations -> MetadataModifier -> m ()
buildSchemaCacheWithInvalidations cacheInvalidations MetadataModifier {..} = do
  metadata <- getMetadata
  let modifiedMetadata = runMetadataModifier metadata
  buildSchemaCacheWithOptions
    (CatalogUpdate mempty)
    cacheInvalidations
    modifiedMetadata
    Nothing
  putMetadata modifiedMetadata

buildSchemaCache :: (MetadataM m, CacheRWM m) => MetadataModifier -> m ()
buildSchemaCache = buildSchemaCacheWithInvalidations mempty

-- | Rebuilds the schema cache after modifying metadata and returns any _new_ metadata inconsistencies.
-- If there are any new inconsistencies, the changes to the metadata and the schema cache are abandoned.
tryBuildSchemaCache ::
  (CacheRWM m, MetadataM m) =>
  MetadataModifier ->
  m (HashMap MetadataObjId (NonEmpty InconsistentMetadata))
tryBuildSchemaCache MetadataModifier {..} =
  tryBuildSchemaCacheWithModifiers [pure . runMetadataModifier]

-- | Rebuilds the schema cache after modifying metadata sequentially and returns any _new_ metadata inconsistencies.
-- If there are any new inconsistencies, the changes to the metadata and the schema cache are abandoned.
-- If the metadata modifiers run into validation issues (e.g. a native query is already tracked in the metadata),
-- we throw these errors back without changing the metadata and schema cache.
tryBuildSchemaCacheWithModifiers ::
  (CacheRWM m, MetadataM m) =>
  [Metadata -> m Metadata] ->
  m (HashMap MetadataObjId (NonEmpty InconsistentMetadata))
tryBuildSchemaCacheWithModifiers modifiers = do
  modifiedMetadata <- do
    metadata <- getMetadata
    foldM (flip ($)) metadata modifiers

  newInconsistentObjects <-
    tryBuildSchemaCacheWithOptions
      (CatalogUpdate mempty)
      mempty
      modifiedMetadata
      Nothing
      validateNewSchemaCache
  when (newInconsistentObjects == mempty)
    $ putMetadata modifiedMetadata
  pure $ newInconsistentObjects
  where
    validateNewSchemaCache :: SchemaCache -> SchemaCache -> (ValidateNewSchemaCacheResult, HashMap MetadataObjId (NonEmpty InconsistentMetadata))
    validateNewSchemaCache oldSchemaCache newSchemaCache =
      let diffInconsistentObjects = HashMap.difference `on` (groupInconsistentMetadataById . scInconsistentObjs)
          newInconsistentObjects = newSchemaCache `diffInconsistentObjects` oldSchemaCache
       in if newInconsistentObjects == mempty
            then (KeepNewSchemaCache, newInconsistentObjects)
            else (DiscardNewSchemaCache, newInconsistentObjects)

-- | Tries to modify the metadata for all the specified metadata objects. If the modification fails,
-- any objects that directly caused a new metadata inconsistency are removed and the modification
-- is attempted again without those failing objects. The failing objects are raised as warnings
-- in 'MonadWarnings' and the successful objects are returned. If there are metadata inconsistencies
-- that are not directly related to the specified metadata objects, an error is thrown.
tryBuildSchemaCacheAndWarnOnFailingObjects ::
  forall m a.
  (CacheRWM m, MonadWarnings m, QErrM m, MetadataM m) =>
  -- | Function makes a metadata modifier for a metadata object
  (a -> m MetadataModifier) ->
  -- | Warning code to use for failed metadata objects
  WarningCode ->
  -- | Map of metadata objects to apply to metadata using the 'mkMetadataModifier' function
  HashMap MetadataObjId a ->
  -- | Successfully applied metadata objects
  m (HashMap MetadataObjId a)
tryBuildSchemaCacheAndWarnOnFailingObjects mkMetadataModifier warningCode metadataObjects = do
  metadataModifier <- fmap mconcat . traverse mkMetadataModifier $ HashMap.elems metadataObjects
  metadataInconsistencies <- tryBuildSchemaCache metadataModifier

  let inconsistentObjects = HashMap.intersectionWith (,) metadataInconsistencies metadataObjects
  let successfulObjects = HashMap.difference metadataObjects inconsistentObjects

  finalMetadataInconsistencies <-
    if not $ null inconsistentObjects
      then do
        -- Raise warnings for objects that failed to track
        for_ (HashMap.toList inconsistentObjects) $ \(metadataObjId, (inconsistencies, _)) -> do
          let errorReasons = commaSeparated $ imReason <$> inconsistencies
          warn $ MetadataWarning warningCode metadataObjId errorReasons

        -- Try again, this time only with objects that were previously successful
        withoutFailedObjectsMetadataModifier <- fmap mconcat . traverse mkMetadataModifier $ HashMap.elems successfulObjects
        tryBuildSchemaCache withoutFailedObjectsMetadataModifier
      else -- Otherwise just look at the rest of the errors, if any
        pure metadataInconsistencies

  unless (null finalMetadataInconsistencies)
    $ throwError
      (err400 Unexpected "cannot continue due to newly found inconsistent metadata")
        { qeInternal = Just $ ExtraInternal $ toJSON (L.nub . concatMap toList $ HashMap.elems finalMetadataInconsistencies)
        }

  pure successfulObjects

-- | Rebuilds the schema cache after modifying metadata. If an object with the given object id became newly inconsistent,
-- raises an error about it specifically. Otherwise, raises a generic metadata inconsistency error.
buildSchemaCacheFor ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  MetadataObjId ->
  MetadataModifier ->
  m ()
buildSchemaCacheFor objectId metadataModifier = do
  newInconsistentObjects <- tryBuildSchemaCache metadataModifier

  for_ (HashMap.lookup objectId newInconsistentObjects) $ \matchingObjects -> do
    let reasons = commaSeparated $ imReason <$> matchingObjects
    throwError (err400 InvalidConfiguration reasons) {qeInternal = Just $ ExtraInternal $ toJSON matchingObjects}

  unless (null newInconsistentObjects)
    $ throwError
      (err400 Unexpected "cannot continue due to new inconsistent metadata")
        { qeInternal = Just $ ExtraInternal $ toJSON (L.nub . concatMap toList $ HashMap.elems newInconsistentObjects)
        }

-- | Requests the schema cache, and fails if there is any inconsistent metadata.
throwOnInconsistencies :: (QErrM m, CacheRWM m) => m ()
throwOnInconsistencies = do
  sc <- askSchemaCache
  let inconsObjs = scInconsistentObjs sc
  unless (null inconsObjs) $ do
    let err = err400 Unexpected "cannot continue due to inconsistent metadata"
    throwError err {qeInternal = Just $ ExtraInternal $ toJSON inconsObjs}

-- | Executes the given action, and if any new 'InconsistentMetadata's are added to the schema
-- cache as a result of its execution, raises an error.
withNewInconsistentObjsCheck :: (QErrM m, CacheRM m) => m a -> m a
withNewInconsistentObjsCheck action = do
  originalObjects <- scInconsistentObjs <$> askSchemaCache
  result <- action
  currentObjects <- scInconsistentObjs <$> askSchemaCache

  let diffInconsistentObjects = HashMap.difference `on` groupInconsistentMetadataById
      newInconsistentObjects =
        L.uniques $ concatMap toList $ HashMap.elems (currentObjects `diffInconsistentObjects` originalObjects)
  unless (null newInconsistentObjects)
    $ throwError
      (err500 Unexpected "cannot continue due to newly found inconsistent metadata")
        { qeInternal = Just $ ExtraInternal $ toJSON newInconsistentObjects
        }

  pure result

-- | getInconsistentQueryCollections is a helper function that runs the
-- static analysis over the saved queries and reports any inconsistenties
-- with the current schema.
getInconsistentQueryCollections ::
  G.SchemaIntrospection ->
  QueryCollections ->
  ((CollectionName, ListedQuery) -> MetadataObject) ->
  EndpointTrie GQLQueryWithText ->
  [NormalizedQuery] ->
  [InconsistentMetadata]
getInconsistentQueryCollections rs qcs lqToMetadataObj restEndpoints allowLst =
  map (\(o, t) -> InconsistentObject t Nothing o) inconsistentMetaObjs
  where
    inconsistentMetaObjs = lefts $ validateQuery <$> lqLst

    zipLQwithDef :: (CollectionName, CreateCollection) -> [((CollectionName, ListedQuery), [G.ExecutableDefinition G.Name])]
    zipLQwithDef (cName, cc) = map (\lq -> ((cName, lq), (G.getExecutableDefinitions . unGQLQuery . getGQLQuery . _lqQuery $ lq))) lqs
      where
        lqs = _cdQueries . _ccDefinition $ cc

    inAllowList :: [NormalizedQuery] -> (ListedQuery) -> Bool
    inAllowList nativeQueryList (ListedQuery {..}) = any (\nqCode -> unNormalizedQuery nqCode == (unGQLQuery . getGQLQuery) _lqQuery) nativeQueryList

    inRESTEndpoints :: EndpointTrie GQLQueryWithText -> (ListedQuery) -> [Text]
    inRESTEndpoints edTrie lq = map fst $ filter (queryIsFaulty) allQueries
      where
        methodMaps = Trie.elems edTrie
        endpoints = concatMap snd $ concatMap (MultiMap.toList) methodMaps
        allQueries :: [(Text, GQLQueryWithText)]
        allQueries = map (\d -> (unNonEmptyText . unEndpointName . _ceName $ d, _edQuery . _ceDefinition $ d)) endpoints

        queryIsFaulty :: (Text, GQLQueryWithText) -> Bool
        queryIsFaulty (_, gqlQ) = (_lqQuery lq) == gqlQ

    lqLst = concatMap zipLQwithDef (InsOrdHashMap.toList qcs)

    formatError :: (CollectionName, ListedQuery) -> [Text] -> Text
    formatError (cName, lq) allErrs = msgInit <> lToTxt allErrs <> faultyEndpoints <> isInAllowList
      where
        msgInit = "In query collection \"" <> toTxt cName <> "\" the query \"" <> (toTxt . _lqName) lq <> "\" is invalid with the following error(s): "
        lToTxt = dquoteList . reverse
        faultyEndpoints = case inRESTEndpoints restEndpoints lq of
          [] -> ""
          ePoints -> ". This query is being used in the following REST endpoint(s): " <> lToTxt ePoints

        isInAllowList = if inAllowList allowLst lq then ". This query is in allowlist." else ""

    validateQuery (eMeta, eDefs) = do
      -- create the gql request object
      let gqlRequest = GQLReq Nothing (GQLExecDoc eDefs) Nothing

      -- @getSingleOperation@ will do the fragment inlining
      singleOperation <- case getSingleOperation gqlRequest of
        Left err -> throwError (lqToMetadataObj eMeta, formatError eMeta [qeError err])
        Right singleOp -> Right singleOp

      -- perform the validation
      for_ (diagnoseGraphQLQuery rs singleOperation) \errors ->
        throwError (lqToMetadataObj eMeta, formatError eMeta errors)

data StoredIntrospection = StoredIntrospection
  { -- Just catalog introspection - not including enums
    siBackendIntrospection :: HashMap SourceName EncJSON,
    siRemotes :: HashMap RemoteSchemaName EncJSON
  }
  deriving stock (Generic)

-- Note that we don't want to introduce an `Eq EncJSON` instance, as this is a
-- bit of a footgun. But for Stored Introspection purposes, it's fine: the
-- worst-case effect of a semantically inaccurate `Eq` instance is that we
-- rebuild the Schema Cache too often.
--
-- However, this does mean that we have to spell out this instance a bit.
instance Eq StoredIntrospection where
  StoredIntrospection bs1 rs1 == StoredIntrospection bs2 rs2 =
    (encJToLBS <$> bs1) == (encJToLBS <$> bs2) && (encJToLBS <$> rs1) == (encJToLBS <$> rs2)

instance FromJSON StoredIntrospection where
  parseJSON = genericParseJSON hasuraJSON

instance ToJSON StoredIntrospection where
  toJSON = genericToJSON hasuraJSON

-- | Represents remote schema or source introspection data to be persisted in a storage (database).
data StoredIntrospectionItem
  = SourceIntrospectionItem SourceName EncJSON
  | RemoteSchemaIntrospectionItem RemoteSchemaName EncJSON

-- The same comment as above for `Eq StoredIntrospection` applies here as well: our refusal to have an `Eq EncJSON` instance means that we can't `stock`-derive this instance.
instance Eq StoredIntrospectionItem where
  SourceIntrospectionItem lSource lIntrospection == SourceIntrospectionItem rSource rIntrospection =
    (lSource == rSource) && (encJToLBS lIntrospection) == (encJToLBS rIntrospection)
  RemoteSchemaIntrospectionItem lRemoteSchema lIntrospection == RemoteSchemaIntrospectionItem rRemoteSchema rIntrospection =
    (lRemoteSchema == rRemoteSchema) && (encJToLBS lIntrospection) == (encJToLBS rIntrospection)
  _ == _ = False

instance Show StoredIntrospectionItem where
  show = \case
    SourceIntrospectionItem sourceName _ -> "introspection data of source " ++ show sourceName
    RemoteSchemaIntrospectionItem remoteSchemaName _ -> "introspection data of source " ++ show remoteSchemaName

-- | Items to be collected while building schema cache
-- See @'buildSchemaCacheRule' for more details.
data CollectItem
  = CollectInconsistentMetadata InconsistentMetadata
  | CollectMetadataDependency MetadataDependency
  | CollectStoredIntrospection StoredIntrospectionItem
  deriving (Show, Eq)
