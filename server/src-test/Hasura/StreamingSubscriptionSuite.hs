{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Hasura.StreamingSubscriptionSuite (buildStreamingSubscriptionSuite) where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM qualified as STM
import Control.Immortal qualified as Immortal
import Control.Lens ((.~))
import Data.Aeson qualified as A
import Data.ByteString.Lazy.UTF8 qualified as LBS
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PG.Query qualified as PG
import Hasura.Backends.Postgres.Connection
import Hasura.Backends.Postgres.Execute.Subscription (MultiplexedQuery (MultiplexedQuery))
import Hasura.Backends.Postgres.Instances.Transport ()
import Hasura.Backends.Postgres.SQL.Value (TxtEncodedVal (TELit))
import Hasura.Base.Error (showQErr)
import Hasura.GraphQL.Execute.Subscription.Options (mkSubscriptionsOptions)
import Hasura.GraphQL.Execute.Subscription.Plan
import Hasura.GraphQL.Execute.Subscription.Poll.Common
import Hasura.GraphQL.Execute.Subscription.Poll.StreamingQuery (pollStreamingQuery)
import Hasura.GraphQL.Execute.Subscription.State
import Hasura.GraphQL.Execute.Subscription.TMap qualified as TMap
import Hasura.GraphQL.ParameterizedQueryHash
import Hasura.GraphQL.Transport.WebSocket.Protocol (unsafeMkOperationId)
import Hasura.GraphQL.Transport.WebSocket.Server qualified as WS
import Hasura.Logging
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend
import Hasura.Server.Init (considerEnv, databaseUrlOption, runWithEnv, _envVar)
import Hasura.Server.Metrics (createServerMetrics)
import Hasura.Server.Prometheus (makeDummyPrometheusMetrics)
import Hasura.Server.Types (RequestId (..), ResizePoolStrategy (..))
import Hasura.Session (RoleName, mkRoleName)
import Language.GraphQL.Draft.Syntax.QQ qualified as G
import ListT qualified
import StmContainers.Map qualified as STMMap
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import System.Metrics
import Test.Hspec

buildStreamingSubscriptionSuite :: IO Spec
buildStreamingSubscriptionSuite = do
  env <- getEnvironment

  pgUrlText :: Text <- flip onLeft (printErrExit . T.pack) $
    runWithEnv env $ do
      let envVar = _envVar databaseUrlOption
      maybeV <- considerEnv envVar
      onNothing maybeV $
        throwError $ "Expected: " <> envVar

  let pgConnInfo = PG.ConnInfo 1 $ PG.CDDatabaseURI $ txtToBs pgUrlText

  pgPool <- PG.initPGPool pgConnInfo PG.defaultConnParams print

  let pgContext = mkPGExecCtx PG.ReadCommitted pgPool NeverResizePool
      dbSourceConfig = PGSourceConfig pgContext pgConnInfo Nothing (pure ()) defaultPostgresExtensionsSchema

  pure $
    describe "Streaming subscriptions polling tests" $ streamingSubscriptionPollingSpec dbSourceConfig

mkRoleNameE :: Text -> RoleName
mkRoleNameE = fromMaybe (error "Use a non empty string") . mkRoleName

-- | CohortStaticSnapshot is a data type to help to compare two cohorts at a specified instant.
--   Note that two Cohorts cannot be directly compared because they have mutable references within
--   them.
data CohortStaticSnapshot = CohortStaticSnapshot
  { _cssCohortId :: CohortId,
    _cssExistingSubscribers :: [SubscriberId],
    _cssNewSubscribers :: [SubscriberId]
  }
  deriving (Show, Eq)

getStaticCohortSnapshot :: Cohort (STM.TVar CursorVariableValues) -> STM.STM CohortStaticSnapshot
getStaticCohortSnapshot (Cohort cohortId _respRef existingSubsTV newSubsTV _) = do
  existingSubs <- TMap.toList existingSubsTV
  newSubs <- TMap.toList newSubsTV
  pure $ CohortStaticSnapshot cohortId (fst <$> existingSubs) (fst <$> newSubs)

streamingSubscriptionPollingSpec :: SourceConfig ('Postgres 'Vanilla) -> Spec
streamingSubscriptionPollingSpec srcConfig = do
  let setupDDLTx =
        PG.unitQE
          defaultTxErrorHandler
          [PG.sql|
            CREATE TABLE "public"."numbers" (id int primary key);
          |]
          ()
          False

      setupValueTx =
        PG.unitQE
          defaultTxErrorHandler
          [PG.sql|
            INSERT INTO "public"."numbers" values (1),(2),(3),(4),(5);
          |]
          ()
          False

      teardownDDLTx =
        PG.unitQE
          defaultTxErrorHandler
          [PG.sql|
              DROP TABLE "public"."numbers";
         |]
          ()
          False

  let setup = do
        runPgSourceWriteTx srcConfig setupDDLTx >>= (`onLeft` (printErrExit . showQErr))
        runPgSourceWriteTx srcConfig setupValueTx >>= (`onLeft` (printErrExit . showQErr))

      teardown =
        runPgSourceWriteTx srcConfig teardownDDLTx >>= (`onLeft` (printErrExit . showQErr))

  runIO setup

  pollerId <- runIO $ PollerId <$> UUID.nextRandom
  let defaultSubscriptionOptions = mkSubscriptionsOptions Nothing Nothing -- use default values
      paramQueryHash = mkUnsafeParameterizedQueryHash "random"
      -- hardcoded multiplexed query which is generated for the following GraphQL query:
      --
      -- { subscription { numbers_stream(cursor: {initial_value: {id: 1}}, batch_size: 1) { id }} }
      multiplexedQuery =
        [PG.sql|
           SELECT  "_subs"."result_id" , "_fld_resp"."root" AS "result", "_fld_resp"."cursor" AS "cursor" FROM UNNEST(($1)::uuid[], ($2)::json[]) AS "_subs"("result_id", "result_vars")
           LEFT OUTER JOIN LATERAL
           (SELECT  json_build_object('numbers_stream', "numbers_stream"."root" ) AS "root", to_json("numbers_stream"."cursor" ) AS "cursor"
            FROM
              (SELECT  coalesce(json_agg("root" ORDER BY "root.pg.id" ASC ), '[]' ) AS "root", json_build_object('id', (MAX("root.pg.id" ))::text ) AS "cursor"
              FROM
              (SELECT  row_to_json((SELECT  "_1_e"  FROM  (SELECT  "_0_root.base"."id" AS "id"       ) AS "_1_e"      ) ) AS "root", "_0_root.base"."id" AS "root.pg.id"
              FROM  (SELECT  *  FROM "public"."numbers"  WHERE (("public"."numbers"."id") > ((("_subs"."result_vars"#>>ARRAY['cursor', 'id']))::integer))   ORDER BY "id" ASC  LIMIT 1 ) AS "_0_root.base"
              ORDER BY "root.pg.id" ASC   ) AS "_2_root"      ) AS "numbers_stream"      )
              AS "_fld_resp" ON ('true')
        |]
  let pollingAction cohortMap testSyncAction =
        pollStreamingQuery
          @('Postgres 'Vanilla)
          pollerId
          defaultSubscriptionOptions
          (SNDefault, srcConfig)
          (mkRoleNameE "random")
          paramQueryHash
          (MultiplexedQuery multiplexedQuery)
          cohortMap
          [G.name|randomRootField|]
          (const $ pure ())
          testSyncAction

      mkSubscriber sId =
        let wsId = maybe (error "Invalid UUID") WS.mkUnsafeWSId $ UUID.fromString "ec981f92-8d5a-47ab-a306-80af7cfb1113"
            requestId = RequestId "randomRequestId"
            subscriberMetadata = mkSubscriberMetadata wsId (unsafeMkOperationId "randomOperationId") Nothing requestId
         in Subscriber sId subscriberMetadata requestId Nothing (const (pure ()))

      mkNewCohort cohortId initialCursorValue = do
        latestCursorValues <- STM.newTVar (CursorVariableValues initialCursorValue)
        Cohort cohortId <$> STM.newTVar Nothing <*> TMap.new <*> TMap.new <*> pure latestCursorValues

      addSubscriberToCohort subscriber cohort =
        TMap.insert subscriber (_sId subscriber) $ _cNewSubscribers cohort

      -- The multiplexed query we chose only deals with cursor variables, so we might as
      -- well create a new function with the other variables being set to `mempty`
      mkCohortVariables' = mkCohortVariables mempty mempty mempty mempty

  describe "Streaming subcription poll" $ do
    cohortId1 <- runIO newCohortId
    (subscriberId1, subscriberId2) <- runIO $ (,) <$> newSubscriberId <*> newSubscriberId
    let subscriber1 = mkSubscriber subscriberId1
        subscriber2 = mkSubscriber subscriberId2
    let initialCursorValue = Map.singleton Name._id (TELit "1")
    cohort1 <- runIO $
      liftIO $
        STM.atomically $ do
          cohort1' <- mkNewCohort cohortId1 initialCursorValue
          -- adding a subscriber to the newly created cohort
          addSubscriberToCohort subscriber1 cohort1'
          pure cohort1'
    cohortMap <- runIO $ liftIO $ STM.atomically $ TMap.new
    let cohortKey1 = mkCohortVariables' (mkUnsafeValidateVariables initialCursorValue)
    cohortId2 <- runIO newCohortId
    cohort2 <- runIO $
      liftIO $
        STM.atomically $ do
          cohort2' <- mkNewCohort cohortId2 initialCursorValue
          addSubscriberToCohort subscriber2 cohort2'
          pure cohort2'

    let mkCohortKey n = cohortKey1 & cvCursorVariables . unValidatedVariables . ix [G.name|id|] .~ TELit n
        cohortKey2 = mkCohortKey "2"
        cohortKey3 = mkCohortKey "3"

    describe "after first poll, the key of the cohort should be updated to contain the next cursor value" $ do
      runIO $
        STM.atomically $ do
          TMap.reset cohortMap
          TMap.insert cohort1 cohortKey1 cohortMap

      runIO $ pollingAction cohortMap Nothing
      currentCohortMap <- runIO $ STM.atomically $ TMap.getMap cohortMap

      it "the key of the cohort1 should have been moved from the cohortKey1 to cohortKey2, so it should not be found anymore at cohortKey1" $ do
        cohortMappedToCohortKey1 <- STM.atomically $ traverse getStaticCohortSnapshot $ Map.lookup cohortKey1 currentCohortMap
        cohortMappedToCohortKey1 `shouldBe` Nothing

      it "the key of the cohort1 should have been moved from the cohortKey1 to cohortKey2, so it should be found anymore at cohortKey2" $ do
        cohortMappedToCohortKey2 <- STM.atomically $ traverse getStaticCohortSnapshot $ Map.lookup cohortKey2 currentCohortMap
        cohortMappedToCohortKey2 `shouldBe` Just (CohortStaticSnapshot cohortId1 [subscriberId1] mempty)

    describe "manipulating cohorts" $ do
      it "adding a new cohort concurrently to the poll, should leave the new cohort as it is in the cohort map" $ do
        syncMVar <- MVar.newEmptyMVar -- MVar to synchronize code
        STM.atomically $ do
          TMap.reset cohortMap
          TMap.insert cohort1 cohortKey1 cohortMap
        let syncAction = do
              -- This action will block until the `syncMVar` is not empty. This action
              -- is run after the cohorts are snapshotted in `pollStreamingQuery`
              MVar.readMVar syncMVar
              STM.atomically $ TMap.insert cohort2 cohortKey3 cohortMap
        Async.withAsync
          (pollingAction cohortMap (Just syncAction))
          ( \pollAsync -> do
              MVar.putMVar syncMVar ()
              Async.wait pollAsync
          )
        currentCohortMap <- STM.atomically $ TMap.getMap cohortMap
        let currentCohort2 = Map.lookup cohortKey3 currentCohortMap

        (originalCohort2StaticSnapshot, currentCohort2StaticSnapshot) <-
          STM.atomically $
            (,) <$> getStaticCohortSnapshot cohort2 <*> traverse getStaticCohortSnapshot currentCohort2
        Just originalCohort2StaticSnapshot `shouldBe` currentCohort2StaticSnapshot

      it "deleting a cohort concurrently should not retain the deleted cohort in the cohort map" $ do
        STM.atomically $ do
          -- reset the cohort map and add a single cohort
          TMap.reset cohortMap
          TMap.insert cohort2 cohortKey1 cohortMap
        syncMVar <- MVar.newEmptyMVar
        let syncAction = do
              MVar.readMVar syncMVar
              STM.atomically $ TMap.delete cohortKey1 cohortMap
        Async.withAsync
          (pollingAction cohortMap (Just syncAction))
          ( \pollAsync -> do
              MVar.putMVar syncMVar ()
              Async.wait pollAsync
          )
        currentCohortMap <- STM.atomically $ TMap.getMap cohortMap
        Map.size currentCohortMap `shouldBe` 0 -- since there was only one cohort initially, now the cohort map should not have any cohorts
    describe "manipulating adding and deleting of subscribers concurrently" $ do
      it "adding a new subscriber concurrently should place the subscriber in the appropriate cohort" $ do
        temporarySubscriberId <- newSubscriberId
        let newTemporarySubscriber = mkSubscriber temporarySubscriberId
        STM.atomically $ do
          TMap.reset cohortMap
          TMap.insert cohort1 cohortKey1 cohortMap
        syncMVar <- MVar.newEmptyMVar
        let syncAction = do
              MVar.readMVar syncMVar
              STM.atomically $ addSubscriberToCohort newTemporarySubscriber cohort1
        Async.withAsync
          (pollingAction cohortMap (Just syncAction))
          ( \pollAsync -> do
              -- concurrently inserting a new cohort to a key (cohortKey2) to which
              -- cohort1 is expected to be associated after the current poll
              MVar.putMVar syncMVar ()
              Async.wait pollAsync
          )
        currentCohortMap <- STM.atomically $ TMap.getMap cohortMap
        let cohortKey2Cohort = Map.lookup cohortKey2 currentCohortMap
            cohortKey1Cohort = Map.lookup cohortKey1 currentCohortMap
        cohortKey1CohortSnapshot <- STM.atomically $ traverse getStaticCohortSnapshot cohortKey1Cohort
        _cssNewSubscribers <$> cohortKey1CohortSnapshot `shouldBe` Just [temporarySubscriberId]

        -- Checking below that the newly added subscriber is not added in the updated cohort
        cohortKey2CohortSnapshot <- STM.atomically $ traverse getStaticCohortSnapshot cohortKey2Cohort
        _cssNewSubscribers <$> cohortKey2CohortSnapshot `shouldSatisfy` all (notElem temporarySubscriberId)
        _cssExistingSubscribers <$> cohortKey2CohortSnapshot `shouldSatisfy` all (notElem temporarySubscriberId)
        STM.atomically $
          TMap.delete temporarySubscriberId (_cNewSubscribers cohort1)

      it "deleting a subscriber from a cohort should not retain the subscriber in any of the cohorts" $ do
        temporarySubscriberId <- newSubscriberId
        let newTemporarySubscriber = mkSubscriber temporarySubscriberId
        STM.atomically $ do
          TMap.reset cohortMap
          TMap.insert cohort1 cohortKey1 cohortMap
          addSubscriberToCohort newTemporarySubscriber cohort1
        syncMVar <- MVar.newEmptyMVar
        let syncAction = do
              MVar.readMVar syncMVar
              STM.atomically $ TMap.delete temporarySubscriberId (_cNewSubscribers cohort1)
        Async.withAsync
          (pollingAction cohortMap (Just syncAction))
          ( \pollAsync -> do
              MVar.putMVar syncMVar ()
              Async.wait pollAsync
          )
        currentCohortMap <- STM.atomically $ TMap.getMap cohortMap
        let cohortKey2Cohort = Map.lookup cohortKey2 currentCohortMap
            cohortKey1Cohort = Map.lookup cohortKey1 currentCohortMap
        cohortKey1CohortSnapshot <- STM.atomically $ traverse getStaticCohortSnapshot cohortKey1Cohort
        cohortKey2CohortSnapshot <- STM.atomically $ traverse getStaticCohortSnapshot cohortKey2Cohort

        -- check the deleted subscriber in the older cohort
        _cssExistingSubscribers <$> cohortKey1CohortSnapshot
          `shouldSatisfy` (\existingSubs -> temporarySubscriberId `notElem` concat (maybeToList existingSubs))
        _cssNewSubscribers <$> cohortKey1CohortSnapshot
          `shouldSatisfy` (\newSubs -> temporarySubscriberId `notElem` concat (maybeToList newSubs))
        _cssExistingSubscribers <$> cohortKey2CohortSnapshot
          `shouldSatisfy` (\existingSubs -> temporarySubscriberId `notElem` concat (maybeToList existingSubs))
        _cssNewSubscribers <$> cohortKey2CohortSnapshot
          `shouldSatisfy` (\newSubs -> temporarySubscriberId `notElem` concat (maybeToList newSubs))
        STM.atomically $
          TMap.delete temporarySubscriberId (_cNewSubscribers cohort1)

    describe "Adding two subscribers concurrently" $ do
      dummyServerStore <- runIO newStore
      dummyServerMetrics <- runIO $ createServerMetrics dummyServerStore
      dummyPromMetrics <- runIO makeDummyPrometheusMetrics

      subscriptionState <- do
        let subOptions = mkSubscriptionsOptions Nothing Nothing
        runIO $ initSubscriptionsState subOptions subOptions (const (pure ()))

      let requestId1 = RequestId "request-id1"
          requestId2 = RequestId "request-id2"

      dummyWSId <- runIO $ WS.mkUnsafeWSId <$> UUID.nextRandom

      let parameterizedSubscriptionQueryPlan = ParameterizedSubscriptionQueryPlan (mkRoleNameE "user") $ MultiplexedQuery multiplexedQuery
          opID1 = unsafeMkOperationId "1"
          opID2 = unsafeMkOperationId "2"
          subscriber1Metadata = mkSubscriberMetadata dummyWSId opID1 Nothing requestId1
          subscriber2Metadata = mkSubscriberMetadata dummyWSId opID2 Nothing requestId2
          dummyParamQueryHash = mkUnsafeParameterizedQueryHash "unsafeParamHash"

          subscriptionQueryPlan =
            SubscriptionQueryPlan
              parameterizedSubscriptionQueryPlan
              srcConfig
              cohortKey1
              Nothing

      let logger :: Logger Hasura = Logger $ \l -> do
            let (logLevel, logType :: EngineLogType Hasura, logDetail) = toEngineLog l
            t <- liftIO $ getFormattedTime Nothing
            liftIO $ putStrLn $ LBS.toString $ A.encode $ EngineLog t logLevel logType logDetail

          addStreamSubQuery subscriberMetadata reqId =
            addStreamSubscriptionQuery
              @('Postgres 'Vanilla)
              logger
              dummyServerMetrics
              dummyPromMetrics
              subscriberMetadata
              subscriptionState
              SNDefault
              dummyParamQueryHash
              Nothing
              reqId
              [G.name|numbers_stream|]
              subscriptionQueryPlan
              (const (pure ()))

      it "concurrently adding two subscribers should retain both of them in the poller map" $ do
        -- Adding two subscribers that query identical queries should be adding them into the same
        -- cohort of the same poller
        liftIO $
          Async.concurrently_
            (addStreamSubQuery subscriber1Metadata requestId1)
            (addStreamSubQuery subscriber2Metadata requestId2)

        let streamQueryMap = _ssStreamQueryMap subscriptionState

        streamQueryMapEntries <- STM.atomically $ ListT.toList $ STMMap.listT streamQueryMap
        length streamQueryMapEntries `shouldBe` 1
        let (pollerKey, (Poller currentCohortMap ioState)) = head streamQueryMapEntries
        cohorts <- STM.atomically $ TMap.toList currentCohortMap
        length cohorts `shouldBe` 1
        let (_cohortKey, Cohort _ _ curSubsTV newSubsTV _) = head cohorts
        allSubscribers <- STM.atomically $ do
          curSubs <- TMap.toList curSubsTV
          newSubs <- TMap.toList newSubsTV
          pure $ curSubs <> newSubs
        length allSubscribers `shouldBe` 2

        threadRef <- STM.atomically $ do
          STMMap.delete pollerKey streamQueryMap
          _pThread <$> STM.readTMVar ioState
        Immortal.stop threadRef

  runIO teardown

printErrExit :: Text -> IO a
printErrExit = (*> exitFailure) . T.putStrLn
