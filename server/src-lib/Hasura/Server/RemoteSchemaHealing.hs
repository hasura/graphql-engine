-- | A background thread that retries introspection of remote schemas which
-- are inconsistent, e.g. because they were unreachable when the schema cache
-- was built at startup. See Note [Remote schema healing].
module Hasura.Server.RemoteSchemaHealing
  ( startRemoteSchemaHealingThread,

    -- * Exported for testing
    HealingEntry (..),
    maxBackoff,
    inconsistentRemoteSchemas,
    newEntry,
    nextBackoff,
  )
where

import Control.Concurrent.Async.Lifted.Safe qualified as LA
import Control.Concurrent.Extended qualified as C
import Control.Immortal qualified as Immortal
import Control.Lens (at, use, (.=), (?=))
import Control.Monad.Loops qualified as L
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Managed (ManagedT)
import Data.Aeson qualified as J
import Data.Bifunctor (bimap)
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.HashSet qualified as HS
import Data.List (partition)
import Data.Text.Extended ((<<>), (<>>))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Clock
import Hasura.App.State
import Hasura.Base.Error
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Schema (runCacheRWT)
import Hasura.RQL.DDL.Schema.Cache.Config
import Hasura.RQL.Types.Metadata (Metadata (..))
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source (MonadResolveSource)
import Hasura.RemoteSchema.Metadata
import Hasura.RemoteSchema.SchemaCache.Build (addRemoteSchemaP2Setup)
import Hasura.Server.AppStateRef
  ( AppStateRef,
    getAppContext,
    getRebuildableSchemaCacheWithVersion,
    getSchemaCache,
    withSchemaCacheUpdate,
  )
import Hasura.Server.Init.Config (OptionalInterval (..))
import Hasura.Services
import Hasura.Tracing qualified as Tracing
import Refined (unrefine)

{- Note [Remote schema healing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a remote schema can't be introspected when the schema cache is built (e.g.
graphql-engine won a startup race against the remote server, or the remote
was briefly down during a metadata reload), it is recorded as inconsistent and
left out of the GraphQL schema. Because the result of 'buildRemoteSchemas' is
cached, nothing re-fetches it until its invalidation key is bumped, which
historically required a manual @reload_remote_schema@ or @reload_metadata@.

This thread wakes every @HASURA_GRAPHQL_REMOTE_SCHEMA_HEALING_INTERVAL@ ms,
looks for inconsistent remote schemas in the current schema cache, and for
each one that is due:

1. Probes the remote by fetching and validating its introspection, *without*
   holding the schema cache lock. The schema cache lock is held for the
   entire duration of a rebuild (including the HTTP fetch), so we don't want
   to take it just to find out the remote is still down. The probe uses the
   remote schema's configured @timeout_seconds@ (default 60s), so a remote
   that hangs only delays this thread, not anything waiting on the lock.

2. If any probes succeeded, takes the lock and rebuilds the schema cache
   locally, invalidating just those remote schemas. This mirrors what the
   schema sync processor does ('Hasura.Server.SchemaUpdate.refreshSchemaCache')
   rather than going through the @reload_remote_schema@ metadata API, which
   would bump the metadata resource version and cause every replica to reload.
   Each replica heals itself based on its own view of the remote.

Each inconsistent remote schema has its own exponential backoff, starting at
the polling interval and capped at 'maxBackoff'. We never give up: nearly all
failure modes (DNS, connection refused, 5xx from a load balancer, partially
deployed schemas...) can be transient, and a failing probe is cheap. We warn
once when a remote schema's backoff reaches the cap.

The backoff is reset if the remote schema's metadata definition changes, so
that a fix made by an admin is retried promptly.
-}

-- | Upper bound on the per-remote-schema retry delay.
maxBackoff :: DiffTime
maxBackoff = seconds 120

-- | Per-remote-schema healing state.
data HealingEntry = HealingEntry
  { -- | The metadata definition of the remote schema, as recorded in the
    -- inconsistency. Used to reset the backoff if the definition changes.
    heDefinition :: J.Value,
    -- | When the remote schema was first seen as inconsistent.
    heFirstSeen :: UTCTime,
    -- | Number of failed attempts to heal so far.
    heAttempts :: Int,
    -- | Delay to wait after the next failed attempt.
    heDelay :: DiffTime,
    -- | Don't attempt again before this time.
    heNextAttempt :: UTCTime,
    -- | Whether we've already logged a warning about reaching 'maxBackoff'.
    heWarnedAtCap :: Bool
  }
  deriving (Show, Eq)

newEntry :: DiffTime -> UTCTime -> J.Value -> HealingEntry
newEntry interval now definition =
  HealingEntry
    { heDefinition = definition,
      heFirstSeen = now,
      heAttempts = 0,
      heDelay = interval,
      heNextAttempt = now,
      heWarnedAtCap = False
    }

-- | Record a failed attempt, scheduling the next one and doubling the delay
-- (up to 'maxBackoff').
nextBackoff :: UTCTime -> HealingEntry -> HealingEntry
nextBackoff now entry =
  entry
    { heAttempts = heAttempts entry + 1,
      heDelay = min maxBackoff (2 * heDelay entry),
      heNextAttempt = Clock.addUTCTime (realToFrac $ heDelay entry) now
    }

-- | The remote schemas that are inconsistent in themselves (as opposed to,
-- say, one of their permissions or relationships), along with the metadata
-- definition recorded in the inconsistency.
--
-- We deliberately only look at 'InconsistentObject', which is what is recorded
-- when fetching or processing the introspection fails, and ignore e.g.
-- 'ConflictingObjects', which retrying won't fix.
inconsistentRemoteSchemas :: [InconsistentMetadata] -> HashMap RemoteSchemaName J.Value
inconsistentRemoteSchemas inconsistencies =
  HashMap.fromList
    [ (name, definition)
    | InconsistentObject _ _ (MetadataObject (MORemoteSchema name) definition) <- inconsistencies
    ]

-- | Starts the remote schema healing thread, unless disabled with 'Skip'.
-- See Note [Remote schema healing].
startRemoteSchemaHealingThread ::
  ( Tracing.MonadTraceContext m,
    C.ForkableMonadIO m,
    HasAppEnv m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    ProvidesNetwork m
  ) =>
  AppStateRef impl ->
  ManagedT m ()
startRemoteSchemaHealingThread appStateRef = do
  AppEnv {..} <- lift askAppEnv
  let logger = _lsLogger appEnvLoggers
  case appEnvRemoteSchemaHealingInterval of
    Skip ->
      logHealing logger LevelInfo "remote schema healing is disabled" J.Null
    Interval interval -> do
      thread <-
        C.forkManagedT "remoteSchemaHealing" logger
          $ L.iterateM_ (healingIteration logger appStateRef (milliseconds $ unrefine interval)) mempty
      logHealing logger LevelInfo "remote schema healing thread started"
        $ J.object
          [ "interval_ms" J..= interval,
            "thread_id" J..= show (Immortal.threadId thread)
          ]

healingIteration ::
  ( Tracing.MonadTraceContext m,
    C.ForkableMonadIO m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    ProvidesNetwork m
  ) =>
  Logger Hasura ->
  AppStateRef impl ->
  DiffTime ->
  HashMap RemoteSchemaName HealingEntry ->
  m (HashMap RemoteSchemaName HealingEntry)
healingIteration logger appStateRef interval oldEntries = do
  liftIO $ C.sleep interval

  schemaCache <- liftIO $ getSchemaCache appStateRef
  now <- liftIO Clock.getCurrentTime
  let recordFailures = traverse_ (recordFailure logger now)
  let inconsistent = inconsistentRemoteSchemas $ scInconsistentObjs schemaCache
      -- Forget remote schemas that are no longer inconsistent (because they
      -- were healed some other way, or removed), and start afresh for those
      -- which are newly inconsistent or whose definition has changed.
      entries = flip HashMap.mapWithKey inconsistent \name definition ->
        case HashMap.lookup name oldEntries of
          Just entry | heDefinition entry == definition -> entry
          _ -> newEntry interval now definition
      due = HashMap.keys $ HashMap.filter ((<= now) . heNextAttempt) entries

  for_ (HashMap.keys $ entries `HashMap.difference` oldEntries) \name ->
    logHealing logger LevelInfo ("remote schema " <> name <<> " is inconsistent; will retry introspecting it periodically") J.Null

  if null due
    then pure entries
    else
      fetchMetadata >>= \case
        Left err -> do
          -- Try again next tick, without counting it against the remote
          -- schemas. Metadata storage errors are reported loudly elsewhere.
          logHealing logger LevelDebug "could not fetch metadata" (J.toJSON err)
          pure entries
        Right (MetadataWithResourceVersion metadata resourceVersion) -> do
          env <- acEnvironment <$> liftIO (getAppContext appStateRef)
          -- Any remote schema not in the metadata has been removed since the
          -- schema cache was built; the schema sync thread will catch up.
          let toProbe = mapMaybe (\name -> (name,) <$> InsOrdHashMap.lookup name (_metaRemoteSchemas metadata)) due
          -- Attempt metadata fetches to see if anything is actually
          -- newly-reachable, before taking the lock in rebuildRemoteSchemas
          probeResults <- LA.forConcurrently toProbe $ probeRemoteSchema env
          let (stillFailing, newlyReachable) = partitionEithers probeResults
          flip execStateT entries do
            -- update stats for still-failing schemas and move on to processing
            -- the newly-reachable ones
            recordFailures stillFailing
            unless (null newlyReachable)
              $ lift (rebuildRemoteSchemas logger appStateRef metadata resourceVersion (HS.fromList newlyReachable))
              >>= \case
                -- Expected to be rare; see 'rebuildRemoteSchemas'. Counted as
                -- a failed attempt so that we back off if it persists.
                Left err ->
                  recordFailures (map (,err) newlyReachable)
                Right stillInconsistent -> do
                  let (notHealed, healed) = partition (`HashMap.member` stillInconsistent) newlyReachable
                  for_ healed \name -> do
                    use (at name) >>= traverse_ \entry ->
                      logHealing logger LevelInfo ("remote schema " <> name <<> " is consistent again")
                        $ J.object
                          [ "attempts" J..= (heAttempts entry + 1),
                            "inconsistent_for_seconds" J..= Clock.diffUTCTime now (heFirstSeen entry)
                          ]
                    at name .= Nothing
                  let err = err500 Unexpected "remote schema is still inconsistent after rebuilding the schema cache"
                  recordFailures (map (,err) notHealed)

-- 'name' ought to always be present in the HashMap
recordFailure ::
  (MonadIO m, MonadState (HashMap RemoteSchemaName HealingEntry) m) =>
  Logger Hasura ->
  UTCTime ->
  (RemoteSchemaName, QErr) ->
  m ()
recordFailure logger now (name, err) =
  use (at name) >>= traverse_ \entry -> do
    let entry' = nextBackoff now entry
        -- warn once, when the wait we're about to schedule first reaches the cap
        reachedCap = heDelay entry >= maxBackoff && not (heWarnedAtCap entry)
    if reachedCap
      then
        logHealing logger LevelWarn ("remote schema " <> name <<> " is still inconsistent; will keep retrying every " <> tshow (diffTimeToSeconds maxBackoff) <> "s")
          $ J.object ["attempts" J..= heAttempts entry', "error" J..= err]
      else
        logHealing logger LevelDebug ("failed to heal remote schema " <>> name)
          $ J.object ["attempts" J..= heAttempts entry', "error" J..= err]
    at name ?= entry' {heWarnedAtCap = heWarnedAtCap entry || reachedCap}
  where
    diffTimeToSeconds :: DiffTime -> Integer
    diffTimeToSeconds = round . realToFrac @DiffTime @Double

-- | Fetch and validate a remote schema's introspection, exactly as the schema
-- cache build does (same headers, URL resolution, timeout and validation).
probeRemoteSchema ::
  (MonadIO m, MonadBaseControl IO m, ProvidesNetwork m) =>
  Env.Environment ->
  (RemoteSchemaName, RemoteSchemaMetadataG r) ->
  m (Either (RemoteSchemaName, QErr) RemoteSchemaName)
probeRemoteSchema env (name, remoteSchema) =
  fmap (bimap (name,) (const name))
    $ runExceptT
    $ Tracing.ignoreTraceT
    $ addRemoteSchemaP2Setup name env (_rsmDefinition remoteSchema)

-- | Rebuild the schema cache, invalidating the given remote schemas.
--
-- Returns the remote schemas which are still inconsistent after the rebuild.
--
-- Fails without rebuilding if the schema cache is not at the given metadata
-- resource version. Usually the schema sync thread is about to catch up, but
-- this can persist if schema sync is disabled. Otherwise fails only if the
-- build itself throws, which shouldn't normally happen since most errors are
-- recorded as inconsistencies instead.
--
-- This duplicates the metadata fetch in probeRemoteSchema but that's okay.
rebuildRemoteSchemas ::
  ( Tracing.MonadTraceContext m,
    MonadIO m,
    MonadBaseControl IO m,
    HasCacheStaticConfig m,
    MonadMetadataStorage m,
    MonadResolveSource m,
    ProvidesNetwork m
  ) =>
  Logger Hasura ->
  AppStateRef impl ->
  Metadata ->
  MetadataResourceVersion ->
  HashSet RemoteSchemaName ->
  m (Either QErr (HashMap RemoteSchemaName J.Value))
rebuildRemoteSchemas logger appStateRef metadata resourceVersion remoteSchemas =
  runExceptT
    $ withSchemaCacheUpdate appStateRef logger Nothing
    $ do
      rebuildableCache <- liftIO $ getRebuildableSchemaCacheWithVersion appStateRef
      dynamicConfig <- buildCacheDynamicConfig <$> liftIO (getAppContext appStateRef)
      (result, cache, _, _, _) <-
        runCacheRWT dynamicConfig rebuildableCache do
          engineResourceVersion <- scMetadataResourceVersion <$> askSchemaCache
          -- The resource version of the schema cache only changes while the
          -- lock is held, so if it matches then 'metadata' is exactly what the
          -- current schema cache was built from, and we won't clobber a newer
          -- build.
          when (engineResourceVersion /= resourceVersion)
            $ throw500 "schema cache is not at the latest metadata resource version"
          buildSchemaCacheWithOptions CatalogSync mempty {ciRemoteSchemas = remoteSchemas} metadata (Just resourceVersion)
          inconsistentRemoteSchemas . scInconsistentObjs <$> askSchemaCache
      pure (result, cache)

logHealing :: (MonadIO m) => Logger Hasura -> LogLevel -> Text -> J.Value -> m ()
logHealing (Logger logger) level message detail =
  logger $ RemoteSchemaHealingLog level message detail
