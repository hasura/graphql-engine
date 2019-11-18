module Hasura.Server.MigrateSpec (spec) where

import           Hasura.Prelude

import           Data.Either             (isRight)
import           Data.Time.Clock         (getCurrentTime)
import           Test.Hspec

import qualified Database.PG.Query       as Q

import           Hasura.Db               (PGExecCtx (..))
import           Hasura.RQL.DDL.Metadata (ClearMetadata (..), runClearMetadata)
import           Hasura.RQL.Types        (QErr, emptySchemaCache, successMsg)
import           Hasura.Server.Migrate
import           Hasura.Server.PGDump
import           Hasura.Server.Query     (Run, RunCtx (..), peelRun)

spec :: Q.ConnInfo -> RunCtx -> PGExecCtx -> Spec
spec pgConnInfo adminCtx pgContext = do
  let runAsAdmin :: Run a -> IO (Either QErr a)
      runAsAdmin = runExceptT . fmap fst .
        peelRun emptySchemaCache adminCtx pgContext Q.ReadWrite

      dropAndInit time = runAsAdmin (dropCatalog *> migrateCatalog time)

  describe "migrateCatalog" $ do
    it "initializes the catalog" $ do
      (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized

    it "is idempotent" $ do
      let dumpSchema = runAsAdmin $
            execPGDump (PGDumpReqBody ["--schema-only"] (Just False)) pgConnInfo
      time <- getCurrentTime
      dropAndInit time `shouldReturn` Right MRInitialized
      firstDump <- dumpSchema
      firstDump `shouldSatisfy` isRight
      dropAndInit time `shouldReturn` Right MRInitialized
      secondDump <- dumpSchema
      secondDump `shouldBe` firstDump

  describe "recreateSystemMetadata" $ do
    let dumpMetadata = runAsAdmin $
          execPGDump (PGDumpReqBody ["--schema=hdb_catalog"] (Just False)) pgConnInfo

    it "is idempotent" $ do
      (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized
      firstDump <- dumpMetadata
      firstDump `shouldSatisfy` isRight
      runAsAdmin recreateSystemMetadata `shouldReturn` Right ()
      secondDump <- dumpMetadata
      secondDump `shouldBe` firstDump

    it "does not create any objects affected by ClearMetadata" $ do
      (dropAndInit =<< getCurrentTime) `shouldReturn` Right MRInitialized
      firstDump <- dumpMetadata
      firstDump `shouldSatisfy` isRight
      runAsAdmin (runClearMetadata ClearMetadata) `shouldReturn` Right successMsg
      secondDump <- dumpMetadata
      secondDump `shouldBe` firstDump
