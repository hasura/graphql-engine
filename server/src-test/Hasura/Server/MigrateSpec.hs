{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.MigrateSpec (CacheRefT (..), spec) where

import Control.Concurrent.MVar.Lifted
import Control.Monad.Morph
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Natural ((:~>) (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.Environment qualified as Env
import Data.Time.Clock (getCurrentTime)
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection
import Hasura.Base.Error
import Hasura.Logging
import Hasura.Metadata.Class
import Hasura.Prelude
import Hasura.RQL.DDL.Metadata (ClearMetadata (..), runClearMetadata)
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Cache.Common
import Hasura.RQL.DDL.Schema.LegacyCatalog (recreateSystemMetadata)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.Source
import Hasura.Server.API.PGDump
import Hasura.Server.Init (DowngradeOptions (..))
import Hasura.Server.Migrate
import Hasura.Server.Types
import Hasura.Session
import Network.HTTP.Client.Manager qualified as HTTP
import Test.Hspec.Core.Spec
import Test.Hspec.Expectations.Lifted

-- -- NOTE: downgrade test disabled for now (see #5273)

newtype CacheRefT m a = CacheRefT {runCacheRefT :: MVar RebuildableSchemaCache -> m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError e,
      MonadBase b,
      MonadBaseControl b,
      MonadTx,
      UserInfoM,
      HTTP.HasHttpManagerM,
      HasServerConfigCtx,
      MonadMetadataStorage,
      MonadMetadataStorageQueryAPI
    )
    via (ReaderT (MVar RebuildableSchemaCache) m)

instance MonadTrans CacheRefT where
  lift = CacheRefT . const

instance MFunctor CacheRefT where
  hoist f (CacheRefT m) = CacheRefT (f . m)

-- instance (MonadBase IO m) => TableCoreInfoRM 'Postgres (CacheRefT m)
instance (MonadBase IO m) => CacheRM (CacheRefT m) where
  askSchemaCache = CacheRefT (fmap lastBuiltSchemaCache . readMVar)

instance
  ( MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m,
    HTTP.HasHttpManagerM m,
    MonadResolveSource m,
    HasServerConfigCtx m
  ) =>
  CacheRWM (CacheRefT m)
  where
  buildSchemaCacheWithOptions reason invalidations metadata =
    CacheRefT $ flip modifyMVar \schemaCache -> do
      ((), cache, _) <- runCacheRWT schemaCache (buildSchemaCacheWithOptions reason invalidations metadata)
      pure (cache, ())

  setMetadataResourceVersionInSchemaCache resourceVersion =
    CacheRefT $ flip modifyMVar \schemaCache -> do
      ((), cache, _) <- runCacheRWT schemaCache (setMetadataResourceVersionInSchemaCache resourceVersion)
      pure (cache, ())

instance Example (MetadataT (CacheRefT m) ()) where
  type Arg (MetadataT (CacheRefT m) ()) = MetadataT (CacheRefT m) :~> IO
  evaluateExample m params action = evaluateExample (action ($$ m)) params ($ ())

type SpecWithCache m = SpecWith (MetadataT (CacheRefT m) :~> IO)

singleTransaction :: MetadataT (CacheRefT m) () -> MetadataT (CacheRefT m) ()
singleTransaction = id

spec ::
  forall m.
  ( MonadIO m,
    MonadBaseControl IO m,
    HTTP.HasHttpManagerM m,
    HasServerConfigCtx m,
    MonadResolveSource m,
    MonadMetadataStorageQueryAPI m
  ) =>
  PostgresConnConfiguration ->
  PGExecCtx ->
  Q.ConnInfo ->
  SpecWithCache m
spec srcConfig pgExecCtx pgConnInfo = do
  let logger :: Logger Hasura = Logger $ \l -> do
        let (logLevel, logType :: EngineLogType Hasura, logDetail) = toEngineLog l
        t <- liftIO $ getFormattedTime Nothing
        liftIO $ putStrLn $ LBS.toString $ encode $ EngineLog t logLevel logType logDetail

      migrateCatalogAndBuildCache env time = do
        (migrationResult, metadata) <- runTx' pgExecCtx $ migrateCatalog (Just srcConfig) MaintenanceModeDisabled time
        (,migrationResult) <$> runCacheBuildM (buildRebuildableSchemaCache logger env metadata)

      dropAndInit env time = lift $
        CacheRefT $ flip modifyMVar \_ ->
          (runTx' pgExecCtx dropHdbCatalogSchema) *> (migrateCatalogAndBuildCache env time)
      downgradeTo v = runTx' pgExecCtx . downgradeCatalog (Just srcConfig) DowngradeOptions {dgoDryRun = False, dgoTargetVersion = v}

  describe "migrateCatalog" $ do
    it "initializes the catalog" $ singleTransaction do
      env <- liftIO Env.getEnvironment
      time <- liftIO getCurrentTime
      dropAndInit env time `shouldReturn` MRInitialized

    it "is idempotent" \(NT transact) -> do
      let dumpSchema = execPGDump (PGDumpReqBody defaultSource ["--schema-only"] False) pgConnInfo
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      firstDump <- transact dumpSchema
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      secondDump <- transact dumpSchema
      secondDump `shouldBe` firstDump

    it "supports upgrades after downgrade to version 12" \(NT transact) -> do
      let upgradeToLatest env time = lift $
            CacheRefT $ flip modifyMVar \_ ->
              migrateCatalogAndBuildCache env time
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      downgradeResult <- (transact . lift) (downgradeTo "12" time)
      downgradeResult `shouldSatisfy` \case
        MRMigrated {} -> True
        _ -> False
      transact (upgradeToLatest env time) `shouldReturn` MRMigrated "12"

  -- -- NOTE: this has been problematic in CI and we're not quite sure how to
  -- --       make this work reliably given the way we do releases and create
  -- --       beta tags and so on. Phil and Alexis are okay just commenting
  -- --       this until we need revisit. See #5273:
  -- it "supports downgrades for every Git tag" $ singleTransaction do
  --   gitOutput <- liftIO $ readProcess "git" ["log", "--no-walk", "--tags", "--pretty=%D"] ""
  --   let filterOldest = filter (not . isPrefixOf "v1.0.0-alpha")
  --       extractTagName = Safe.headMay . splitOn ", " <=< stripPrefix "tag: "
  --       supportedDowngrades = sort (map fst downgradeShortcuts)
  --       gitTags = (sort . filterOldest . mapMaybe extractTagName . tail . lines) gitOutput
  --   for_ gitTags \t ->
  --     t `shouldSatisfy` (`elem` supportedDowngrades)

  describe "recreateSystemMetadata" $ do
    let dumpMetadata = execPGDump (PGDumpReqBody defaultSource ["--schema=hdb_catalog"] False) pgConnInfo

    it "is idempotent" \(NT transact) -> do
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      -- Downgrade to catalog version before metadata separation
      downgradeResult <- (transact . lift) (downgradeTo "42" time)
      downgradeResult `shouldSatisfy` \case
        MRMigrated {} -> True
        _ -> False
      firstDump <- transact dumpMetadata
      transact (runTx' pgExecCtx recreateSystemMetadata)
      secondDump <- transact dumpMetadata
      secondDump `shouldBe` firstDump

    it "does not create any objects affected by ClearMetadata" \(NT transact) -> do
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      firstDump <- transact dumpMetadata
      transact (flip runReaderT logger $ runClearMetadata ClearMetadata) `shouldReturn` successMsg
      secondDump <- transact dumpMetadata
      secondDump `shouldBe` firstDump

runTx' ::
  (MonadError QErr m, MonadIO m, MonadBaseControl IO m) =>
  PGExecCtx ->
  Q.TxET QErr m a ->
  m a
runTx' pgExecCtx = liftEitherM . runExceptT . runTx pgExecCtx Q.ReadWrite
