module Hasura.RQL.DDL.Metadata
  ( runReplaceMetadata,
    runReplaceMetadataV2,
    runExportMetadata,
    runExportMetadataV2,
    runClearMetadata,
    runReloadMetadata,
    runDumpInternalState,
    runGetInconsistentMetadata,
    runDropInconsistentMetadata,
    runGetCatalogState,
    runSetCatalogState,
    runTestWebhookTransform,
    runSetMetricsConfig,
    runRemoveMetricsConfig,
    module Hasura.RQL.DDL.Metadata.Types,
  )
where

import Control.Lens ((.~), (^.), (^?))
import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as AO
import Data.Attoparsec.Text qualified as AT
import Data.Bifunctor (bimap, first)
import Data.Bitraversable
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Environment qualified as Env
import Data.Has (Has, getter)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.TByteString qualified as TBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Extended ((<<>))
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Logging qualified as HL
import Hasura.Metadata.Class
import Hasura.Prelude hiding (first)
import Hasura.RQL.DDL.Action
import Hasura.RQL.DDL.ComputedField
import Hasura.RQL.DDL.CustomTypes
import Hasura.RQL.DDL.Endpoint
import Hasura.RQL.DDL.EventTrigger
import Hasura.RQL.DDL.InheritedRoles
import Hasura.RQL.DDL.Metadata.Types
import Hasura.RQL.DDL.Network
import Hasura.RQL.DDL.Permission
import Hasura.RQL.DDL.Relationship
import Hasura.RQL.DDL.RemoteRelationship
import Hasura.RQL.DDL.RemoteSchema
import Hasura.RQL.DDL.ScheduledTrigger
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.WebhookTransforms
import Hasura.RQL.Types
import Hasura.RQL.Types.Eventing.Backend (BackendEventTrigger (..))
import Hasura.SQL.AnyBackend qualified as AB
import Kriti qualified as K
import Network.HTTP.Client.Transformable qualified as HTTP

runClearMetadata ::
  ( MonadIO m,
    CacheRWM m,
    MetadataM m,
    MonadMetadataStorageQueryAPI m,
    MonadReader r m,
    Has (HL.Logger HL.Hasura) r
  ) =>
  ClearMetadata ->
  m EncJSON
runClearMetadata _ = do
  metadata <- getMetadata
  -- We can infer whether the server is started with `--database-url` option
  -- (or corresponding env variable) by checking the existence of @'defaultSource'
  -- in current metadata.
  let maybeDefaultSourceMetadata = metadata ^? metaSources . ix defaultSource
      emptyMetadata' = case maybeDefaultSourceMetadata of
        Nothing -> emptyMetadata
        Just exists ->
          -- If default postgres source is defined, we need to set metadata
          -- which contains only default source without any tables and functions.
          let emptyDefaultSource =
                AB.dispatchAnyBackend @Backend exists \(s :: SourceMetadata b) ->
                  AB.mkAnyBackend @b $
                    SourceMetadata @b defaultSource mempty mempty (_smConfiguration @b s) Nothing emptySourceCustomization
           in emptyMetadata
                & metaSources %~ OMap.insert defaultSource emptyDefaultSource
  runReplaceMetadataV1 $ RMWithSources emptyMetadata'

{- Note [Cleanup for dropped triggers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There was an issue (https://github.com/hasura/graphql-engine/issues/5461)
fixed (via https://github.com/hasura/graphql-engine/pull/6137) related to
event triggers while replacing metadata in the catalog prior to metadata
separation. The metadata separation solves the issue naturally, since the
'hdb_catalog.event_triggers' table is no more in use and new/updated event
triggers are processed in building schema cache. But we need to drop the
database trigger and archive events for dropped event triggers. This is handled
explicitly in @'runReplaceMetadata' function.
-}

-- | Replace the 'current metadata' with the 'new metadata'
-- The 'new metadata' might come via the 'Import Metadata' in console
runReplaceMetadata ::
  ( CacheRWM m,
    MetadataM m,
    MonadIO m,
    MonadMetadataStorageQueryAPI m,
    MonadReader r m,
    Has (HL.Logger HL.Hasura) r
  ) =>
  ReplaceMetadata ->
  m EncJSON
runReplaceMetadata = \case
  RMReplaceMetadataV1 v1args -> runReplaceMetadataV1 v1args
  RMReplaceMetadataV2 v2args -> runReplaceMetadataV2 v2args

runReplaceMetadataV1 ::
  ( QErrM m,
    CacheRWM m,
    MetadataM m,
    MonadIO m,
    MonadMetadataStorageQueryAPI m,
    MonadReader r m,
    Has (HL.Logger HL.Hasura) r
  ) =>
  ReplaceMetadataV1 ->
  m EncJSON
runReplaceMetadataV1 =
  (successMsg <$) . runReplaceMetadataV2 . ReplaceMetadataV2 NoAllowInconsistentMetadata

runReplaceMetadataV2 ::
  forall m r.
  ( QErrM m,
    CacheRWM m,
    MetadataM m,
    MonadIO m,
    MonadMetadataStorageQueryAPI m,
    MonadReader r m,
    Has (HL.Logger HL.Hasura) r
  ) =>
  ReplaceMetadataV2 ->
  m EncJSON
runReplaceMetadataV2 ReplaceMetadataV2 {..} = do
  logger :: (HL.Logger HL.Hasura) <- asks getter
  -- we drop all the future cron trigger events before inserting the new metadata
  -- and re-populating future cron events below
  let introspectionDisabledRoles =
        case _rmv2Metadata of
          RMWithSources m -> _metaSetGraphqlIntrospectionOptions m
          RMWithoutSources _ -> mempty
  oldMetadata <- getMetadata

  (cronTriggersMetadata, cronTriggersToBeAdded) <- processCronTriggers oldMetadata

  metadata <- case _rmv2Metadata of
    RMWithSources m -> pure $ m {_metaCronTriggers = cronTriggersMetadata}
    RMWithoutSources MetadataNoSources {..} -> do
      let maybeDefaultSourceMetadata = oldMetadata ^? metaSources . ix defaultSource . toSourceMetadata
      defaultSourceMetadata <-
        onNothing maybeDefaultSourceMetadata $
          throw400 NotSupported "cannot import metadata without sources since no default source is defined"
      let newDefaultSourceMetadata =
            AB.mkAnyBackend
              defaultSourceMetadata
                { _smTables = _mnsTables,
                  _smFunctions = _mnsFunctions
                }
      pure $
        Metadata
          (OMap.singleton defaultSource newDefaultSourceMetadata)
          _mnsRemoteSchemas
          _mnsQueryCollections
          _mnsAllowlist
          _mnsCustomTypes
          _mnsActions
          cronTriggersMetadata
          (_metaRestEndpoints oldMetadata)
          emptyApiLimit
          emptyMetricsConfig
          mempty
          introspectionDisabledRoles
          emptyNetwork
  putMetadata metadata

  case _rmv2AllowInconsistentMetadata of
    AllowInconsistentMetadata ->
      buildSchemaCache noMetadataModify
    NoAllowInconsistentMetadata ->
      buildSchemaCacheStrict

  -- populate future cron events for all the new cron triggers that are imported
  for_ cronTriggersToBeAdded $ \CronTriggerMetadata {..} ->
    populateInitialCronTriggerEvents ctSchedule ctName

  -- See Note [Cleanup for dropped triggers]
  dropSourceSQLTriggers logger (_metaSources oldMetadata) (_metaSources metadata)

  encJFromJValue . formatInconsistentObjs . scInconsistentObjs <$> askSchemaCache
  where
    {- Note [Cron triggers behaviour with replace metadata]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    When the metadata is replaced, we delete only the cron triggers
    that were deleted, instead of deleting all the old cron triggers (which
    existed in the metadata before it was replaced) and inserting all the
    new cron triggers. This is done this way, because when a cron trigger is
    dropped, the cron events associated with it will also be dropped from the DB
    and when a new cron trigger is added, new cron events are generated by the
    graphql-engine. So, this way we only delete and insert the data which has been changed.

    The cron triggers that were deleted is calculated by getting a diff
    of the old cron triggers and the new cron triggers. Note that we don't just
    check the name of the trigger to calculate the diff, the whole cron trigger
    definition is considered in the calculation.

    Note: Only cron triggers with `include_in_metadata` set to `true` can be updated/deleted
    via the replace metadata API. Cron triggers with `include_in_metadata` can only be modified
    via the `create_cron_trigger` and `delete_cron_trigger` APIs.

    -}
    processCronTriggers oldMetadata = do
      let (oldCronTriggersIncludedInMetadata, oldCronTriggersNotIncludedInMetadata) =
            OMap.partition ctIncludeInMetadata (_metaCronTriggers oldMetadata)
          allNewCronTriggers =
            case _rmv2Metadata of
              RMWithoutSources m -> _mnsCronTriggers m
              RMWithSources m -> _metaCronTriggers m
          -- this function is intended to use with `Map.differenceWith`, it's used when two
          -- equal keys are encountered, then the values are compared to calculate the diff.
          -- see https://hackage.haskell.org/package/unordered-containers-0.2.14.0/docs/Data-HashMap-Internal.html#v:differenceWith
          leftIfDifferent l r
            | l == r = Nothing
            | otherwise = Just l
          cronTriggersToBeAdded =
            Map.differenceWith
              leftIfDifferent
              (OMap.toHashMap allNewCronTriggers)
              (OMap.toHashMap oldCronTriggersIncludedInMetadata)
          cronTriggersToBeDropped =
            Map.differenceWith
              leftIfDifferent
              (OMap.toHashMap oldCronTriggersIncludedInMetadata)
              (OMap.toHashMap allNewCronTriggers)
      dropFutureCronEvents $ MetadataCronTriggers $ Map.keys cronTriggersToBeDropped
      cronTriggers <- do
        -- traverse over the new cron triggers and check if any of them
        -- already exists as a cron trigger with "included_in_metadata: false"
        for_ allNewCronTriggers $ \ct ->
          when (ctName ct `OMap.member` oldCronTriggersNotIncludedInMetadata) $
            throw400 AlreadyExists $
              "cron trigger with name "
                <> ctName ct
                <<> " already exists as a cron trigger with \"included_in_metadata\" as false"
        -- we add the old cron triggers with included_in_metadata set to false with the
        -- newly added cron triggers
        pure $ allNewCronTriggers <> oldCronTriggersNotIncludedInMetadata
      pure $ (cronTriggers, cronTriggersToBeAdded)

    dropSourceSQLTriggers ::
      HL.Logger HL.Hasura ->
      InsOrdHashMap SourceName BackendSourceMetadata ->
      InsOrdHashMap SourceName BackendSourceMetadata ->
      m ()
    dropSourceSQLTriggers (HL.Logger logger) oldSources newSources = do
      -- NOTE: the current implementation of this function has an edge case.
      -- The edge case is that when a `SourceA` which contained some event triggers
      -- is modified to point to a new database, this function will try to drop the
      -- SQL triggers of the dropped event triggers on the new database which doesn't exist.
      -- In the current implementation, this doesn't throw an error because the trigger is dropped
      -- using `DROP IF EXISTS..` meaning this silently fails without throwing an error.
      for_ (OMap.toList newSources) $ \(source, newBackendSourceMetadata) -> do
        onJust (OMap.lookup source oldSources) $ \oldBackendSourceMetadata ->
          compose source newBackendSourceMetadata oldBackendSourceMetadata \(newSourceMetadata :: SourceMetadata b) -> do
            dispatch oldBackendSourceMetadata \oldSourceMetadata -> do
              let oldTriggersMap = getTriggersMap oldSourceMetadata
                  newTriggersMap = getTriggersMap newSourceMetadata
                  droppedTriggers = OMap.keys $ oldTriggersMap `OMap.difference` newTriggersMap
                  catcher e@QErr {qeCode}
                    | qeCode == Unexpected = pure () -- NOTE: This information should be returned by the inconsistent_metadata response, so doesn't need additional logging.
                    | otherwise = throwError e -- rethrow other errors

              -- This will swallow Unexpected exceptions for sources if allow_inconsistent_metadata is enabled
              -- This should be ok since if the sources are already missing from the cache then they should
              -- not need to be removed.
              --
              -- TODO: Determine if any errors should be thrown from askSourceConfig at all if the errors are just being discarded
              return $
                flip catchError catcher do
                  sourceConfig <- askSourceConfig @b source
                  for_ droppedTriggers $ dropTriggerAndArchiveEvents @b sourceConfig
      where
        getTriggersMap = OMap.unions . map _tmEventTriggers . OMap.elems . _smTables

        dispatch = AB.dispatchAnyBackend @BackendEventTrigger

        compose ::
          SourceName ->
          AB.AnyBackend i ->
          AB.AnyBackend i ->
          (forall b. BackendEventTrigger b => i b -> i b -> m ()) ->
          m ()
        compose sourceName x y f = AB.composeAnyBackend @BackendEventTrigger f x y (logger $ HL.UnstructuredLog HL.LevelInfo $ TBS.fromText $ "Event trigger clean up couldn't be done on the source " <> sourceName <<> " because it has changed its type")

-- | Only includes the cron triggers with `included_in_metadata` set to `True`
processCronTriggersMetadata :: Metadata -> Metadata
processCronTriggersMetadata metadata =
  let cronTriggersIncludedInMetadata = OMap.filter ctIncludeInMetadata $ _metaCronTriggers metadata
   in metadata {_metaCronTriggers = cronTriggersIncludedInMetadata}

runExportMetadata ::
  forall m.
  (QErrM m, MetadataM m) =>
  ExportMetadata ->
  m EncJSON
runExportMetadata ExportMetadata {} =
  encJFromOrderedValue . metadataToOrdJSON <$> (processCronTriggersMetadata <$> getMetadata)

runExportMetadataV2 ::
  forall m.
  (QErrM m, MetadataM m) =>
  MetadataResourceVersion ->
  ExportMetadata ->
  m EncJSON
runExportMetadataV2 currentResourceVersion ExportMetadata {} = do
  exportMetadata <- processCronTriggersMetadata <$> getMetadata
  pure $
    encJFromOrderedValue $
      AO.object
        [ ("resource_version", AO.toOrdered currentResourceVersion),
          ("metadata", metadataToOrdJSON exportMetadata)
        ]

runReloadMetadata :: (QErrM m, CacheRWM m, MetadataM m) => ReloadMetadata -> m EncJSON
runReloadMetadata (ReloadMetadata reloadRemoteSchemas reloadSources reloadRecreateEventTriggers) = do
  metadata <- getMetadata
  let allSources = HS.fromList $ OMap.keys $ _metaSources metadata
      allRemoteSchemas = HS.fromList $ OMap.keys $ _metaRemoteSchemas metadata
      checkRemoteSchema name =
        unless (HS.member name allRemoteSchemas) $
          throw400 NotExists $
            "Remote schema with name " <> name <<> " not found in metadata"
      checkSource name =
        unless (HS.member name allSources) $
          throw400 NotExists $
            "Source with name " <> name <<> " not found in metadata"

  remoteSchemaInvalidations <- case reloadRemoteSchemas of
    RSReloadAll -> pure allRemoteSchemas
    RSReloadList l -> mapM_ checkRemoteSchema l *> pure l
  pgSourcesInvalidations <- case reloadSources of
    RSReloadAll -> pure allSources
    RSReloadList l -> mapM_ checkSource l *> pure l
  recreateEventTriggersSources <- case reloadRecreateEventTriggers of
    RSReloadAll -> pure allSources
    RSReloadList l -> mapM_ checkSource l *> pure l

  let cacheInvalidations =
        CacheInvalidations
          { ciMetadata = True,
            ciRemoteSchemas = remoteSchemaInvalidations,
            ciSources = pgSourcesInvalidations
          }

  buildSchemaCacheWithOptions (CatalogUpdate $ Just recreateEventTriggersSources) cacheInvalidations metadata
  pure successMsg

runDumpInternalState ::
  (QErrM m, CacheRM m) =>
  DumpInternalState ->
  m EncJSON
runDumpInternalState _ =
  encJFromJValue <$> askSchemaCache

runGetInconsistentMetadata ::
  (QErrM m, CacheRM m) =>
  GetInconsistentMetadata ->
  m EncJSON
runGetInconsistentMetadata _ = do
  inconsObjs <- scInconsistentObjs <$> askSchemaCache
  return $ encJFromJValue $ formatInconsistentObjs inconsObjs

formatInconsistentObjs :: [InconsistentMetadata] -> J.Value
formatInconsistentObjs inconsObjs =
  J.object
    [ "is_consistent" J..= null inconsObjs,
      "inconsistent_objects" J..= inconsObjs
    ]

runDropInconsistentMetadata ::
  (QErrM m, CacheRWM m, MetadataM m) =>
  DropInconsistentMetadata ->
  m EncJSON
runDropInconsistentMetadata _ = do
  sc <- askSchemaCache
  let inconsSchObjs = L.nub . concatMap imObjectIds $ scInconsistentObjs sc
  -- Note: when building the schema cache, we try to put dependents after their dependencies in the
  -- list of inconsistent objects, so reverse the list to start with dependents first. This is not
  -- perfect — a completely accurate solution would require performing a topological sort — but it
  -- seems to work well enough for now.
  metadataModifier <- execWriterT $ mapM_ (tell . purgeMetadataObj) (reverse inconsSchObjs)
  metadata <- getMetadata
  putMetadata $ unMetadataModifier metadataModifier metadata
  buildSchemaCache noMetadataModify
  -- after building the schema cache, we need to check the inconsistent metadata, if any
  -- are only those which are not droppable
  newInconsistentObjects <- scInconsistentObjs <$> askSchemaCache
  let droppableInconsistentObjects = filter droppableInconsistentMetadata newInconsistentObjects
  unless (null droppableInconsistentObjects) $
    throwError
      (err400 Unexpected "cannot continue due to new inconsistent metadata")
        { qeInternal = Just $ ExtraInternal $ J.toJSON newInconsistentObjects
        }
  return successMsg

purgeMetadataObj :: MetadataObjId -> MetadataModifier
purgeMetadataObj = \case
  MOSource source -> MetadataModifier $ metaSources %~ OMap.delete source
  MOSourceObjId source exists -> AB.dispatchAnyBackend @BackendMetadata exists $ handleSourceObj source
  MORemoteSchema rsn -> dropRemoteSchemaInMetadata rsn
  MORemoteSchemaPermissions rsName role -> dropRemoteSchemaPermissionInMetadata rsName role
  MOCustomTypes -> clearCustomTypesInMetadata
  MOAction action -> dropActionInMetadata action -- Nothing
  MOActionPermission action role -> dropActionPermissionInMetadata action role
  MOCronTrigger ctName -> dropCronTriggerInMetadata ctName
  MOEndpoint epName -> dropEndpointInMetadata epName
  MOInheritedRole role -> dropInheritedRoleInMetadata role
  MOHostTlsAllowlist host -> dropHostFromAllowList host
  where
    handleSourceObj :: forall b. BackendMetadata b => SourceName -> SourceMetadataObjId b -> MetadataModifier
    handleSourceObj source = \case
      SMOTable qt -> dropTableInMetadata @b source qt
      SMOFunction qf -> dropFunctionInMetadata @b source qf
      SMOFunctionPermission qf rn -> dropFunctionPermissionInMetadata @b source qf rn
      SMOTableObj qt tableObj ->
        MetadataModifier $
          tableMetadataSetter @b source qt %~ case tableObj of
            MTORel rn _ -> dropRelationshipInMetadata rn
            MTOPerm rn pt -> dropPermissionInMetadata rn pt
            MTOTrigger trn -> dropEventTriggerInMetadata trn
            MTOComputedField ccn -> dropComputedFieldInMetadata ccn
            MTORemoteRelationship rn -> dropRemoteRelationshipInMetadata rn

runGetCatalogState ::
  (MonadMetadataStorageQueryAPI m) => GetCatalogState -> m EncJSON
runGetCatalogState _ =
  encJFromJValue <$> fetchCatalogState

runSetCatalogState ::
  (MonadMetadataStorageQueryAPI m) => SetCatalogState -> m EncJSON
runSetCatalogState SetCatalogState {..} = do
  updateCatalogState _scsType _scsState
  pure successMsg

runSetMetricsConfig ::
  (MonadIO m, CacheRWM m, MetadataM m, MonadError QErr m) =>
  MetricsConfig ->
  m EncJSON
runSetMetricsConfig mc = do
  withNewInconsistentObjsCheck $
    buildSchemaCache $
      MetadataModifier $
        metaMetricsConfig .~ mc
  pure successMsg

runRemoveMetricsConfig ::
  (MonadIO m, CacheRWM m, MetadataM m, MonadError QErr m) =>
  m EncJSON
runRemoveMetricsConfig = do
  withNewInconsistentObjsCheck $
    buildSchemaCache $
      MetadataModifier $
        metaMetricsConfig .~ emptyMetricsConfig
  pure successMsg

data TestTransformError
  = UrlInterpError K.RenderedError
  | RequestInitializationError HTTP.HttpException
  | RequestTransformationError HTTP.Request TransformErrorBundle

runTestWebhookTransform ::
  (QErrM m) =>
  TestWebhookTransform ->
  m EncJSON
runTestWebhookTransform (TestWebhookTransform env urlE payload mt _ sv) = do
  url <- case urlE of
    URL url' ->
      case AT.parseOnly parseEnvTemplate url' of
        Left _ -> throwError $ err400 ParseFailed "Invalid Url Template"
        Right xs ->
          let lookup' var = maybe (Left var) (Right . T.pack) $ Env.lookupEnv env (T.unpack var)
              result = traverse (fmap indistinct . bitraverse lookup' pure) xs
              err e = throwError $ err400 NotFound $ "Missing Env Var: " <> e
           in either err (pure . fold) result
    EnvVar var ->
      let err = throwError $ err400 NotFound "Missing Env Var"
       in maybe err (pure . T.pack) $ Env.lookupEnv env var

  result <- runExceptT $ do
    let env' = bimap T.pack (J.String . T.pack) <$> Env.toList env
        decodeKritiResult = TE.decodeUtf8 . BL.toStrict . J.encode

    kritiUrlResult <- hoistEither $ first UrlInterpError $ decodeKritiResult <$> K.runKriti (TE.encodeUtf8 $ "\"" <> url <> "\"") env'

    let unwrappedUrl = T.drop 1 $ T.dropEnd 1 kritiUrlResult
    initReq <- hoistEither $ first RequestInitializationError $ HTTP.mkRequestEither unwrappedUrl

    let req = initReq & HTTP.body .~ pure (J.encode payload)
        reqTransform = mkRequestTransform mt
        reqTransformCtx = buildReqTransformCtx unwrappedUrl sv
    hoistEither $ first (RequestTransformationError req) $ applyRequestTransform reqTransformCtx reqTransform req

  case result of
    Right transformed ->
      let body = J.toJSON $ J.decode @J.Value =<< (transformed ^. HTTP.body)
       in pure $ packTransformResult transformed body
    Left (RequestTransformationError req err) -> pure $ packTransformResult req (J.toJSON err)
    -- NOTE: In the following two cases we have failed before producing a valid request.
    Left (UrlInterpError err) -> pure $ encJFromJValue $ J.toJSON err
    Left (RequestInitializationError err) -> pure $ encJFromJValue $ J.String $ "Error: " <> tshow err

parseEnvTemplate :: AT.Parser [Either T.Text T.Text]
parseEnvTemplate = AT.many1 $ pEnv <|> pLit <|> fmap Right "{"
  where
    pEnv = fmap (Left) $ "{{" *> AT.takeWhile1 (/= '}') <* "}}"
    pLit = fmap Right $ AT.takeWhile1 (/= '{')

indistinct :: Either a a -> a
indistinct = either id id

packTransformResult :: HTTP.Request -> J.Value -> EncJSON
packTransformResult req body =
  encJFromJValue $
    J.object
      [ "webhook_url" J..= (req ^. HTTP.url),
        "method" J..= (req ^. HTTP.method),
        "headers" J..= (first CI.foldedCase <$> (req ^. HTTP.headers)),
        "body" J..= body
      ]
