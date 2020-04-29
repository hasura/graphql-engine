{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.MigrateSpec (CacheRefT(..), spec) where

import           Hasura.Prelude

import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Control.Monad.Unique
import           Control.Natural                ((:~>) (..))
import           Data.List                      (isPrefixOf, stripPrefix)
import           Data.List.Split                (splitOn)
import           Data.Time.Clock                (getCurrentTime)
import           Data.Tuple                     (swap)
import           System.Process                 (readProcess)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Expectations.Lifted

import qualified Database.PG.Query              as Q
import qualified Safe

import           Hasura.RQL.DDL.Metadata        (ClearMetadata (..), runClearMetadata)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.API.PGDump
import           Hasura.Server.Init             (DowngradeOptions (..), downgradeShortcuts)
import           Hasura.Server.Migrate
import           Hasura.Server.Version          (HasVersion)

newtype CacheRefT m a
  = CacheRefT { runCacheRefT :: MVar (RebuildableSchemaCache m) -> m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadError e, MonadBase b, MonadBaseControl b
    , MonadTx, MonadUnique, UserInfoM, HasHttpManager, HasSQLGenCtx )
    via (ReaderT (MVar (RebuildableSchemaCache m)) m)

instance MonadTrans CacheRefT where
  lift = CacheRefT . const

instance (MonadBase IO m) => TableCoreInfoRM (CacheRefT m)
instance (MonadBase IO m) => CacheRM (CacheRefT m) where
  askSchemaCache = CacheRefT (fmap lastBuiltSchemaCache . readMVar)

instance (MonadIO m, MonadBaseControl IO m, MonadTx m) => CacheRWM (CacheRefT m) where
  buildSchemaCacheWithOptions reason invalidations = CacheRefT $ flip modifyMVar \schemaCache -> do
    ((), cache, _) <- runCacheRWT schemaCache (buildSchemaCacheWithOptions reason invalidations)
    pure (cache, ())

instance Example (CacheRefT m ()) where
  type Arg (CacheRefT m ()) = CacheRefT m :~> IO
  evaluateExample m params action = evaluateExample (action ($$ m)) params ($ ())

type SpecWithCache m = SpecWith (CacheRefT m :~> IO)

singleTransaction :: CacheRefT m () -> CacheRefT m ()
singleTransaction = id

spec
  :: ( HasVersion
     , MonadIO m
     , MonadBaseControl IO m
     , MonadTx m
     , MonadUnique m
     , HasHttpManager m
     , HasSQLGenCtx m
     )
  => Q.ConnInfo -> SpecWithCache m
spec pgConnInfo = do
  let dropAndInit time = CacheRefT $ flip modifyMVar \_ ->
        dropCatalog *> (swap <$> migrateCatalog time)

  describe "migrateCatalog" $ do
    it "initializes the catalog" $ singleTransaction do
      (dropAndInit =<< liftIO getCurrentTime) `shouldReturn` MRInitialized

    it "is idempotent" \(NT transact) -> do
      let dumpSchema = execPGDump (PGDumpReqBody ["--schema-only"] (Just False)) pgConnInfo
      time <- getCurrentTime
      transact (dropAndInit time) `shouldReturn` MRInitialized
      firstDump <- transact dumpSchema
      transact (dropAndInit time) `shouldReturn` MRInitialized
      secondDump <- transact dumpSchema
      secondDump `shouldBe` firstDump

    it "supports upgrades after downgrade to version 12" \(NT transact) -> do
      let downgradeTo v = downgradeCatalog DowngradeOptions{ dgoDryRun = False, dgoTargetVersion = v }
          upgradeToLatest time = CacheRefT $ flip modifyMVar \_ ->
            swap <$> migrateCatalog time
      time <- getCurrentTime
      transact (dropAndInit time) `shouldReturn` MRInitialized
      downgradeResult <- (transact . lift) (downgradeTo "12" time)
      downgradeResult `shouldSatisfy` \case
        MRMigrated{} -> True
        _ -> False
      transact (upgradeToLatest time) `shouldReturn` MRMigrated "12"

    it "supports downgrades for every Git tag" $ singleTransaction do
      gitOutput <- liftIO $ readProcess "git" ["log", "--no-walk", "--tags", "--pretty=%D"] ""
      let filterOldest = filter (not . isPrefixOf "v1.0.0-alpha")
          extractTagName = Safe.headMay . splitOn ", " <=< stripPrefix "tag: "
          supportedDowngrades = sort (map fst downgradeShortcuts)
          gitTags = (sort . filterOldest . mapMaybe extractTagName . tail . lines) gitOutput
      for_ gitTags \t ->
        t `shouldSatisfy` (`elem` supportedDowngrades)

  describe "recreateSystemMetadata" $ do
    let dumpMetadata = execPGDump (PGDumpReqBody ["--schema=hdb_catalog"] (Just False)) pgConnInfo

    it "is idempotent" \(NT transact) -> do
      (transact . dropAndInit =<< getCurrentTime) `shouldReturn` MRInitialized
      firstDump <- transact dumpMetadata
      transact recreateSystemMetadata
      secondDump <- transact dumpMetadata
      secondDump `shouldBe` firstDump

    it "does not create any objects affected by ClearMetadata" \(NT transact) -> do
      (transact . dropAndInit =<< getCurrentTime) `shouldReturn` MRInitialized
      firstDump <- transact dumpMetadata
      transact (runClearMetadata ClearMetadata) `shouldReturn` successMsg
      secondDump <- transact dumpMetadata
      secondDump `shouldBe` firstDump
