{-# LANGUAGE UndecidableInstances #-}

module Hasura.Server.MigrateSpec (CacheRefT(..), spec) where

import           Hasura.Prelude

import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Control.Monad.Unique
import           Control.Natural                ((:~>) (..))
import           Data.Time.Clock                (getCurrentTime)
import           Data.Tuple                     (swap)
import           Test.Hspec.Core.Spec
import           Test.Hspec.Expectations.Lifted

import qualified Database.PG.Query              as Q

import           Hasura.RQL.DDL.Metadata        (ClearMetadata (..), runClearMetadata)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.Types
import           Hasura.Server.Migrate
import           Hasura.Server.PGDump

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

instance (MonadIO m, MonadBaseControl IO m, MonadTx m, MonadUnique m) => CacheRWM (CacheRefT m) where
  buildSchemaCacheWithOptions options = CacheRefT $ flip modifyMVar \schemaCache ->
    swap <$> runCacheRWT schemaCache (buildSchemaCacheWithOptions options)
  invalidateCachedRemoteSchema name = CacheRefT $ flip modifyMVar \schemaCache ->
    swap <$> runCacheRWT schemaCache (invalidateCachedRemoteSchema name)

instance Example (CacheRefT m ()) where
  type Arg (CacheRefT m ()) = CacheRefT m :~> IO
  evaluateExample m params action = evaluateExample (action ($$ m)) params ($ ())

type SpecWithCache m = SpecWith (CacheRefT m :~> IO)

singleTransaction :: CacheRefT m () -> CacheRefT m ()
singleTransaction = id

spec
  :: ( MonadIO m
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
      let dumpSchema = transact $
            execPGDump (PGDumpReqBody ["--schema-only"] (Just False)) pgConnInfo
      time <- getCurrentTime
      transact (dropAndInit time) `shouldReturn` MRInitialized
      firstDump <- dumpSchema
      transact (dropAndInit time) `shouldReturn` MRInitialized
      secondDump <- dumpSchema
      secondDump `shouldBe` firstDump

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
