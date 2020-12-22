{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.MigrateSpec (CacheRefT(..), spec) where

import           Hasura.Prelude

import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Control.Monad.Unique
import           Control.Natural                     ((:~>) (..))
import           Data.Time.Clock                     (getCurrentTime)
import           Data.Tuple                          (swap)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Expectations.Lifted

import qualified Data.Environment                    as Env
import qualified Database.PG.Query                   as Q

import           Hasura.RQL.DDL.Metadata             (ClearMetadata (..), runClearMetadata)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DDL.Schema.LegacyCatalog
import           Hasura.RQL.Types
import           Hasura.Server.API.PGDump
import           Hasura.Server.Init                  (DowngradeOptions (..))
import           Hasura.Server.Migrate
import           Hasura.Server.Version               (HasVersion)

-- -- NOTE: downgrade test disabled for now (see #5273)

newtype CacheRefT m a
  = CacheRefT { runCacheRefT :: MVar RebuildableSchemaCache -> m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadError e, MonadBase b, MonadBaseControl b
    , MonadTx, MonadUnique, UserInfoM, HasHttpManager, HasSQLGenCtx )
    via (ReaderT (MVar RebuildableSchemaCache) m)

instance MonadTrans CacheRefT where
  lift = CacheRefT . const

instance (MonadBase IO m) => TableCoreInfoRM 'Postgres (CacheRefT m)
instance (MonadBase IO m) => CacheRM (CacheRefT m) where
  askSchemaCache = CacheRefT (fmap lastBuiltSchemaCache . readMVar)

instance (MonadIO m, MonadBaseControl IO m, MonadTx m, HasHttpManager m, HasSQLGenCtx m, HasRemoteSchemaPermsCtx m) => CacheRWM (CacheRefT m) where
  buildSchemaCacheWithOptions reason invalidations metadata = CacheRefT $ flip modifyMVar \schemaCache -> do
    ((), cache, _) <- runCacheRWT schemaCache (buildSchemaCacheWithOptions reason invalidations metadata)
    pure (cache, ())

instance Example (MetadataT (CacheRefT m) ()) where
  type Arg (MetadataT (CacheRefT m) ()) = MetadataT (CacheRefT m) :~> IO
  evaluateExample m params action = evaluateExample (action ($$ m)) params ($ ())

type SpecWithCache m = SpecWith (MetadataT (CacheRefT m) :~> IO)

singleTransaction :: MetadataT (CacheRefT m) () -> MetadataT (CacheRefT m) ()
singleTransaction = id

spec
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadTx m
     , HasHttpManager m
     , HasSQLGenCtx m
     , HasRemoteSchemaPermsCtx m
     )
  => Q.ConnInfo -> SpecWithCache m
spec pgConnInfo = do
  let dropAndInit env time = lift $ CacheRefT $ flip modifyMVar \_ ->
        dropCatalog *> (swap <$> migrateCatalog env time)
      downgradeTo v = downgradeCatalog DowngradeOptions{ dgoDryRun = False, dgoTargetVersion = v }

  describe "migrateCatalog" $ do
    it "initializes the catalog" $ singleTransaction do
      env <- liftIO Env.getEnvironment
      time <- liftIO getCurrentTime
      dropAndInit env time `shouldReturn` MRInitialized

    it "is idempotent" \(NT transact) -> do
      let dumpSchema = execPGDump (PGDumpReqBody ["--schema-only"] (Just False)) pgConnInfo
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      firstDump <- transact dumpSchema
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      secondDump <- transact dumpSchema
      secondDump `shouldBe` firstDump

    it "supports upgrades after downgrade to version 12" \(NT transact) -> do
      let upgradeToLatest env time = lift $ CacheRefT $ flip modifyMVar \_ ->
            swap <$> migrateCatalog env time
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      downgradeResult <- (transact . lift) (downgradeTo "12" time)
      downgradeResult `shouldSatisfy` \case
        MRMigrated{} -> True
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
    let dumpMetadata = execPGDump (PGDumpReqBody ["--schema=hdb_catalog"] (Just False)) pgConnInfo

    it "is idempotent" \(NT transact) -> do
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      -- Downgrade to catalog version before metadata separation
      downgradeResult <- (transact . lift) (downgradeTo "42" time)
      downgradeResult `shouldSatisfy` \case
        MRMigrated{} -> True
        _ -> False
      firstDump <- transact dumpMetadata
      transact recreateSystemMetadata
      secondDump <- transact dumpMetadata
      secondDump `shouldBe` firstDump

    it "does not create any objects affected by ClearMetadata" \(NT transact) -> do
      env <- Env.getEnvironment
      time <- getCurrentTime
      transact (dropAndInit env time) `shouldReturn` MRInitialized
      firstDump <- transact dumpMetadata
      transact (runClearMetadata ClearMetadata) `shouldReturn` successMsg
      secondDump <- transact dumpMetadata
      secondDump `shouldBe` firstDump
