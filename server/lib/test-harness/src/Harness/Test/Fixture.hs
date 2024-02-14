{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module defines a way to setup test fixtures which can help defining
-- tests.
--
-- Central types and functions are 'Fixture', 'SetupAction', and 'run'.
module Harness.Test.Fixture
  ( run,
    runClean,
    runSingleSetup,
    runWithLocalTestEnvironment,
    runWithLocalTestEnvironmentSingleSetup,
    runWithLocalTestEnvironmentInternal,
    hgeWithEnv,
    createDatabases,
    Fixture (..),
    fixture,
    FixtureName (..),
    BackendType (..),
    BackendTypeConfig (..),
    pattern DataConnectorMock,
    pattern DataConnectorReference,
    pattern DataConnectorSqlite,
    noLocalTestEnvironment,
    SetupAction (..),
    Options (..),
    combineOptions,
    defaultOptions,
    fixtureRepl,
    combineFixtures,
    LHSFixture,
    RHSFixture,
    withPermissions,
  )
where

import Control.Concurrent.Async qualified as Async
import Control.Monad.Managed (Managed, runManaged, with)
import Data.Aeson (Value)
import Data.List (subsequences)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.UUID.V4 (nextRandom)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.Exceptions
import Harness.GraphqlEngine (postGraphqlInternal, postMetadataInternal, postMetadata_)
import Harness.Logging
import Harness.Permissions (Permission (..))
import Harness.Permissions qualified as Permissions
import Harness.Services.GraphqlEngine
import Harness.Test.BackendType
import Harness.Test.CustomOptions
import Harness.Test.FixtureName
import Harness.Test.SetupAction (SetupAction (..))
import Harness.Test.SetupAction qualified as SetupAction
import Harness.TestEnvironment
  ( GlobalTestEnvironment (..),
    Server (..),
    TestEnvironment (..),
    TestingMode (..),
    TestingRole (..),
    UniqueTestId (..),
    getSchemaNameInternal,
    logger,
  )
import Harness.Yaml
import Hasura.Prelude hiding (log)
import Test.Hspec
  ( ActionWith,
    SpecWith,
    aroundAllWith,
    aroundWith,
    beforeWith,
    describe,
    expectationFailure,
    pendingWith,
  )
import Test.Hspec.Core.Spec (Item (..), mapSpecItem_)
import Text.Show.Pretty (ppShow)

-- | Runs the given tests, for each provided 'Fixture'@ ()@.
--
-- Each 'Fixture' describes how to setup and teardown the state of the system being tested.
-- 'run' guarantees that state setup and teardown is exception safe, and that
-- the teardown actions are run in reverse order of the setup actions.
--
-- See 'Fixture' for details.
--
-- This function restricts the local testEnvironment parameter for 'Fixture' to be '()',
-- indicating that there should be _no_ local testEnvironment.
--
-- For a more general version that can run tests for any 'Fixture'@ a@, see
-- 'runWithLocalTestEnvironment'.
--
-- The commented out version of this function runs setup and teardown for each Spec item individually.
-- however it makes CI punishingly slow, so we defer to the "worse" version for
-- now. When we come to run specs in parallel this will be helpful.
run :: NonEmpty (Fixture ()) -> SpecWith TestEnvironment -> SpecWith GlobalTestEnvironment
run = runSingleSetup

-- this refreshes all the metadata on every test, so we can test mutations etc
-- is much slower though so try `run` first
runClean :: NonEmpty (Fixture ()) -> SpecWith TestEnvironment -> SpecWith GlobalTestEnvironment
runClean fixtures = runWithLocalTestEnvironment fixtures . beforeWith \(te, ()) -> return te

-- given a fresh HgeServerInstance, add it in our `TestEnvironment`
useHgeInTestEnvironment :: GlobalTestEnvironment -> HgeServerInstance -> IO GlobalTestEnvironment
useHgeInTestEnvironment globalTestEnvironment (HgeServerInstance {hgeServerHost, hgeServerPort}) = do
  serverThreadIrrelevant <- Async.async (return ())
  let server =
        Server
          { port = fromIntegral hgeServerPort,
            urlPrefix = "http://" <> T.unpack hgeServerHost,
            thread = serverThreadIrrelevant
          }
  pure $ globalTestEnvironment {server = server}

-- | Start an instance of HGE which has certain environment variables defined
-- and pass it around inside the `GlobalTestEnvironment`
hgeWithEnv :: [(String, String)] -> SpecWith GlobalTestEnvironment -> SpecWith GlobalTestEnvironment
hgeWithEnv env = do
  let hgeConfig = emptyHgeConfig {hgeConfigEnvironmentVars = env}

  aroundAllWith
    ( \specs globalTestEnvironment -> do
        (hgeServerInstance, cleanup) <- spawnServer globalTestEnvironment hgeConfig
        useHgeInTestEnvironment globalTestEnvironment hgeServerInstance >>= specs
        cleanup
    )

{-# DEPRECATED runSingleSetup "runSingleSetup lets all specs in aFixture share a single database environment, which impedes parallelisation and out-of-order execution." #-}
runSingleSetup :: NonEmpty (Fixture ()) -> SpecWith TestEnvironment -> SpecWith GlobalTestEnvironment
runSingleSetup fixtures = runWithLocalTestEnvironmentSingleSetup fixtures . beforeWith \(te, ()) -> return te

-- | Runs the given tests, for each provided 'Fixture'@ a@.
--
-- Each 'Fixture' provides a list of 'SetupActions';
-- 'runWithLocalTestEnvironment' guarantees that the associated 'teardown'
-- function is always called after a setup, even if the tests fail.
--
-- 'Fixture's are parameterized by the type of local testEnvironment that needs
-- to be carried throughout the tests.
--
-- This function runs setup and teardown for each Spec item individually.
--
-- See 'Fixture' for details.
runWithLocalTestEnvironment ::
  forall a.
  NonEmpty (Fixture a) ->
  SpecWith (TestEnvironment, a) ->
  SpecWith GlobalTestEnvironment
runWithLocalTestEnvironment = runWithLocalTestEnvironmentInternal aroundWith

{-# DEPRECATED runWithLocalTestEnvironmentSingleSetup "runWithLocalTestEnvironmentSingleSetup lets all specs in a Fixture share a single database environment, which impedes parallelisation and out-of-order execution." #-}
runWithLocalTestEnvironmentSingleSetup ::
  forall a.
  NonEmpty (Fixture a) ->
  SpecWith (TestEnvironment, a) ->
  SpecWith GlobalTestEnvironment
runWithLocalTestEnvironmentSingleSetup = runWithLocalTestEnvironmentInternal aroundAllWith

runWithLocalTestEnvironmentInternal ::
  forall a.
  ((ActionWith (TestEnvironment, a) -> ActionWith (GlobalTestEnvironment)) -> SpecWith (TestEnvironment, a) -> SpecWith (GlobalTestEnvironment)) ->
  NonEmpty (Fixture a) ->
  SpecWith (TestEnvironment, a) ->
  SpecWith GlobalTestEnvironment
runWithLocalTestEnvironmentInternal aroundSomeWith fixtures tests =
  for_ fixtures \fixture' -> do
    let n = name fixture'
        co = customOptions fixture'
        options = fromMaybe defaultOptions co

        shouldRunIn :: FixtureName -> TestingMode -> Bool
        shouldRunIn fixtureName = \case
          TestNewPostgresVariant {} ->
            Postgres `elem` backendTypesForFixture fixtureName
          TestBackend (DataConnector _) ->
            -- we run all these together so it's harder to miss tests
            any isDataConnector (backendTypesForFixture fixtureName)
          TestBackend backendType ->
            backendType `elem` backendTypesForFixture fixtureName
          TestNoBackends -> S.null (backendTypesForFixture fixtureName)
          TestEverything -> True

    describe (show n) do
      flip aroundSomeWith tests \test globalTestEnvironment ->
        if not (n `shouldRunIn` testingMode globalTestEnvironment)
          then pendingWith $ "Inapplicable test."
          else case skipTests options of
            Just skipMsg -> pendingWith $ "Tests skipped: " <> T.unpack skipMsg
            Nothing -> fixtureBracket fixture' options test globalTestEnvironment

-- We want to be able to report exceptions happening both during the tests
-- and at teardown, which is why we use a custom re-implementation of
-- @bracket@.
fixtureBracket ::
  Fixture b ->
  Options ->
  (ActionWith (TestEnvironment, b)) ->
  ActionWith GlobalTestEnvironment
fixtureBracket
  Fixture
    { name,
      mkLocalTestEnvironment,
      setupTeardown
    }
  options
  actionWith
  globalTestEnvironment =
    mask \restore -> runManaged do
      liftIO $ runLogger (logger globalTestEnvironment) $ LogFixtureTestStart (tshow name)
      -- create databases we need
      testEnvironment <- liftIO $ setupTestEnvironment name globalTestEnvironment options
      -- set up local env for remote schema testing etc
      localTestEnvironment <- mkLocalTestEnvironment testEnvironment
      liftIO $ do
        let testEnvironments = (testEnvironment, localTestEnvironment)

        cleanup <- runSetupActions (logger $ globalEnvironment testEnvironment) (setupTeardown testEnvironments)

        _ <-
          catchRethrow
            (restore $ actionWith testEnvironments)
            cleanup

        -- run test-specific clean up
        cleanup

        -- drop all DBs created for the tests
        dropDatabases name testEnvironment

-- | given the `FixtureName` and `uniqueTestId`, spin up all necessary
-- databases for these tests
createDatabases :: FixtureName -> TestEnvironment -> IO ()
createDatabases fixtureName testEnvironment =
  traverse_
    ( \case
        Postgres ->
          Postgres.createDatabase testEnvironment
        Cockroach ->
          Cockroach.createDatabase testEnvironment
        Citus ->
          Citus.createDatabase testEnvironment
        SQLServer ->
          Sqlserver.createDatabase testEnvironment
        _ -> pure ()
    )
    (backendTypesForFixture fixtureName)

dropDatabases :: FixtureName -> TestEnvironment -> IO ()
dropDatabases fixtureName testEnvironment =
  traverse_
    ( \case
        Postgres ->
          Postgres.dropDatabase testEnvironment
        Cockroach ->
          Cockroach.dropDatabase testEnvironment
        Citus ->
          Citus.dropDatabase testEnvironment
        SQLServer ->
          Sqlserver.dropDatabase testEnvironment
        _ -> pure ()
    )
    (backendTypesForFixture fixtureName)

-- | Tests all run with unique schema names now, so we need to produce a test
-- environment that points to a unique schema name.
setupTestEnvironment :: FixtureName -> GlobalTestEnvironment -> Options -> IO TestEnvironment
setupTestEnvironment name globalTestEnvironment options = do
  uniqueTestId <- UniqueTestId <$> nextRandom

  let testEnvironment =
        TestEnvironment
          { fixtureName = name,
            uniqueTestId = uniqueTestId,
            globalEnvironment = globalTestEnvironment,
            permissions = Admin,
            _options = options,
            _postgraphqlInternal = postGraphqlInternal,
            _postMetadataInternal = postMetadataInternal,
            _shouldReturnYamlFInternal = \testEnv -> withFrozenCallStack $ shouldReturnYamlFInternal (_options testEnv),
            _getSchemaNameInternal = getSchemaNameInternal
          }

  -- create source databases
  createDatabases name testEnvironment
  pure testEnvironment

-- | A function that makes it easy to perform setup and teardown when
-- debugging/developing tests within a repl.
fixtureRepl ::
  Fixture a ->
  GlobalTestEnvironment ->
  IO (IO ())
fixtureRepl Fixture {name, mkLocalTestEnvironment, setupTeardown} globalTestEnvironment = do
  testEnvironment <- setupTestEnvironment name globalTestEnvironment defaultOptions
  with (mkLocalTestEnvironment testEnvironment) \localTestEnvironment -> do
    let testEnvironments = (testEnvironment, localTestEnvironment)
    cleanup <- runSetupActions (logger $ globalEnvironment testEnvironment) (setupTeardown testEnvironments)
    return cleanup

-- | Run a list of SetupActions.
--
-- * If all setup steps complete, return an IO action that runs the teardown actions in reverse order.
-- * If a setup step fails, the steps that were executed are torn down in reverse order.
-- * Teardown always collects all the exceptions that are thrown.
runSetupActions :: Logger -> [SetupAction] -> IO (IO ())
runSetupActions logger acts = go acts []
  where
    log :: forall a. (LoggableMessage a) => a -> IO ()
    log = runLogger logger

    go :: [SetupAction] -> [IO ()] -> IO (IO ())
    go actions cleanupAcc = case actions of
      [] -> return (rethrowAll cleanupAcc)
      SetupAction {setupAction, teardownAction} : rest -> do
        a <- try setupAction
        case a of
          Left (exn :: SomeException) -> do
            log $ LogFixtureSetupFailed (length cleanupAcc)
            rethrowAll
              ( throwIO exn
                  : ( log (LogFixtureTeardownFailed (length cleanupAcc))
                        >> teardownAction Nothing
                    )
                  : cleanupAcc
              )
            return (return ())
          Right x -> do
            log $ LogFixtureSetupSucceeded (length cleanupAcc)
            go
              rest
              ( ( log (LogFixtureTeardownSucceeded (length cleanupAcc))
                    >> teardownAction (Just x)
                )
                  : cleanupAcc
              )

--------------------------------------------------------------------------------

-- | A fixture represents the state of the system-under-test which a set of
-- tests rely on; this could be an individual backend, or a setup of several of
-- them to test relationships.
--
-- The @a@ parameter defines the local testEnvironment, in addition to the
-- global testEnvironment. A test that doesn't require additional local
-- testEnvironment can indicate this with '()'.
--
-- Test-system state is setup via a list of 'SetupAction's.
data Fixture a = Fixture
  { -- | A name describing the given context.
    --
    -- e.g. @Postgres@ or @BigQuery@
    name :: FixtureName,
    -- | Setup actions associated with creating a local testEnvironment for this
    -- 'Fixture'; for example, starting remote servers.
    --
    -- If any of those resources need to be threaded throughout the tests
    -- themselves they should be returned here. Otherwise, use 'noTestResource'.
    --
    -- Intended to be used with the 'Harness.Test.TestResource' module. See
    -- 'Harness.Webhook' for an example.
    mkLocalTestEnvironment :: TestEnvironment -> Managed a,
    -- | Setup actions associated with this 'Fixture'; for example:
    --
    --  * running SQL commands
    --  * sending metadata commands
    --
    -- Takes the global 'TestEnvironment' and any local testEnvironment (i.e. @a@) as arguments.
    setupTeardown :: (TestEnvironment, a) -> [SetupAction],
    -- | Options which modify the behavior of a given testing 'Fixture'; when
    -- this field is 'Nothing', tests are given the 'defaultOptions'.
    customOptions :: Maybe Options
  }

-- | A simple smart constructor for a 'Fixture'.
fixture :: FixtureName -> Fixture ()
fixture name = Fixture {..}
  where
    setupTeardown = const []
    mkLocalTestEnvironment = noLocalTestEnvironment
    customOptions = Nothing

-- | Default function for 'mkLocalTestEnvironment' when there's no local testEnvironment.
noLocalTestEnvironment :: TestEnvironment -> Managed ()
noLocalTestEnvironment = const $ pure ()

-- Each left-hand-side (LHS) fixture is responsible for setting up the remote relationship, and
-- for tearing it down. Each lhs fixture is given the JSON representation for
-- the table name on the RHS.
type LHSFixture = Value -> Fixture (Maybe Server)

-- Each right-hand-side (RHS) fixture is responsible for setting up the target table, and for
-- returning the JSON representation of said table.
type RHSFixture = (Value, Fixture ())

-- | Combines a left-hand side (LHS) and right-hand side (RHS) fixtures.
--
-- The RHS is set up first, then the LHS can create the remote relationship.
--
-- Teardown is done in the reverse order.
--
-- The metadata is cleared befored each setup.
combineFixtures :: [TestEnvironment -> SetupAction] -> LHSFixture -> RHSFixture -> Fixture (Maybe Server)
combineFixtures setupActions lhs (tableName, rhs) =
  (fixture $ Combine lhsName rhsName)
    { mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
      setupTeardown = \(testEnvironment, localTestEnvironment) ->
        [SetupAction.clearMetadata testEnvironment]
          <> ((\mkSetupAction -> mkSetupAction testEnvironment) <$> setupActions)
          <> rhsSetupTeardown (testEnvironment, ())
          <> lhsSetupTeardown (testEnvironment, localTestEnvironment),
      customOptions = combineOptions lhsOptions rhsOptions
    }
  where
    Fixture
      { name = lhsName,
        mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
        setupTeardown = lhsSetupTeardown,
        customOptions = lhsOptions
      } = lhs tableName
    Fixture
      { name = rhsName,
        setupTeardown = rhsSetupTeardown,
        customOptions = rhsOptions
      } = rhs

-- | Assert that a given test block requires precisely the given permissions.
--
-- This function modifies the given Hspec test "forest" to replace each test
-- with two separate tests:
--
-- * The original test, but all requests will be made under a Hasura role with
--   the given permissions. This test should therefore only pass if the given
--   permissions are sufficient.
--
-- * The opposite test: all requests will be made under a Hasura role with
--   every (proper) subset of the given permissions. If the original test can
--   run successfully with missing permissions, then this test will __fail__.
--
-- The responsibility is on the test writer to make the permissions
-- requirements as granular as possible. If two tests in the same block require
-- differing levels of permissions, those tests should be separated into
-- distinct blocks.
--
-- Note that we don't check anything about /extra/ permissions.
withPermissions :: NonEmpty Permission -> SpecWith TestEnvironment -> SpecWith TestEnvironment
withPermissions (toList -> permissions) spec = do
  describe "With sufficient permissions" do
    flip mapSpecItem_ spec \item ->
      item {itemExample = \params -> itemExample item params . succeeding}

  describe "With insufficient permissions" do
    flip mapSpecItem_ spec \item ->
      item {itemExample = \params -> itemExample item params . failing}
  where
    succeeding :: (ActionWith TestEnvironment -> IO ()) -> ActionWith TestEnvironment -> IO ()
    succeeding k test = k \testEnvironment -> do
      for_ permissions
        $ postMetadata_ testEnvironment
        . Permissions.createPermissionMetadata testEnvironment

      test testEnvironment {permissions = NonAdmin permissions} `finally` do
        for_ permissions
          $ postMetadata_ testEnvironment
          . Permissions.dropPermissionMetadata testEnvironment

    failing :: (ActionWith TestEnvironment -> IO ()) -> ActionWith TestEnvironment -> IO ()
    failing k test = k \testEnvironment -> do
      -- Test every possible (strict) subset of the permissions to ensure that
      -- they lead to test failures.
      for_ (subsequences permissions) \subsequence ->
        unless (subsequence == permissions) do
          for_ subsequence
            $ postMetadata_ testEnvironment
            . Permissions.createPermissionMetadata testEnvironment

          let attempt :: IO () -> IO ()
              attempt x =
                try x >>= \case
                  Right _ ->
                    expectationFailure
                      $ mconcat
                        [ "Unexpectedly adequate permissions:\n",
                          ppShow subsequence
                        ]
                  Left (_ :: SomeException) ->
                    pure ()

          attempt (test testEnvironment {permissions = NonAdmin subsequence}) `finally` do
            for_ subsequence
              $ postMetadata_ testEnvironment
              . Permissions.dropPermissionMetadata testEnvironment
