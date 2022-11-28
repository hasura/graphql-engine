{-# LANGUAGE PatternSynonyms #-}

-- | This module defines a way to setup test fixtures which can help defining
-- tests.
--
-- Central types and functions are 'Fixture', 'SetupAction', and 'run'.
module Harness.Test.Fixture
  ( run,
    runSingleSetup,
    runWithLocalTestEnvironment,
    runWithLocalTestEnvironmentSingleSetup,
    Fixture (..),
    fixture,
    FixtureName (..),
    BackendType (..),
    pattern DataConnectorMock,
    pattern DataConnectorReference,
    pattern DataConnectorSqlite,
    defaultSource,
    defaultBackendDisplayNameString,
    defaultBackendTypeString,
    schemaKeyword,
    noLocalTestEnvironment,
    SetupAction (..),
    Options (..),
    combineOptions,
    defaultOptions,
    fixtureRepl,
    combineFixtures,
    LHSFixture,
    RHSFixture,
  )
where

import Control.Monad.Managed (Managed, runManaged, with)
import Data.Aeson (Value)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.UUID.V4 (nextRandom)
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Backend.Sqlserver qualified as Sqlserver
import Harness.Exceptions
import Harness.Logging
import Harness.Test.BackendType
import Harness.Test.CustomOptions
import Harness.Test.SetupAction (SetupAction (..))
import Harness.Test.SetupAction qualified as SetupAction
import Harness.TestEnvironment (Server, TestEnvironment (..), TestingMode (..), testLogHarness)
import Hasura.Prelude hiding (log)
import Test.Hspec
  ( ActionWith,
    SpecWith,
    aroundAllWith,
    aroundWith,
    beforeWith,
    describe,
    pendingWith,
  )

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
run :: NonEmpty (Fixture ()) -> (Options -> SpecWith TestEnvironment) -> SpecWith TestEnvironment
run = runSingleSetup

-- runWithLocalTestEnvironment fixtures (\opts -> beforeWith (\(te, ()) -> return te) (tests opts))

{-# DEPRECATED runSingleSetup "runSingleSetup lets all specs in aFixture share a single database environment, which impedes parallelisation and out-of-order execution." #-}
runSingleSetup :: NonEmpty (Fixture ()) -> (Options -> SpecWith TestEnvironment) -> SpecWith TestEnvironment
runSingleSetup fixtures tests = do
  runWithLocalTestEnvironmentSingleSetup fixtures (\opts -> beforeWith (\(te, ()) -> return te) (tests opts))

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
  (Options -> SpecWith (TestEnvironment, a)) ->
  SpecWith TestEnvironment
runWithLocalTestEnvironment = runWithLocalTestEnvironmentInternal aroundWith

{-# DEPRECATED runWithLocalTestEnvironmentSingleSetup "runWithLocalTestEnvironmentSingleSetup lets all specs in a Fixture share a single database environment, which impedes parallelisation and out-of-order execution." #-}
runWithLocalTestEnvironmentSingleSetup ::
  forall a.
  NonEmpty (Fixture a) ->
  (Options -> SpecWith (TestEnvironment, a)) ->
  SpecWith TestEnvironment
runWithLocalTestEnvironmentSingleSetup = runWithLocalTestEnvironmentInternal aroundAllWith

runWithLocalTestEnvironmentInternal ::
  forall a.
  ((ActionWith (TestEnvironment, a) -> ActionWith (TestEnvironment)) -> SpecWith (TestEnvironment, a) -> SpecWith (TestEnvironment)) ->
  NonEmpty (Fixture a) ->
  (Options -> SpecWith (TestEnvironment, a)) ->
  SpecWith TestEnvironment
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
            let isDataConnector = \case
                  DataConnector _dc -> True
                  _ -> False
             in any isDataConnector (backendTypesForFixture fixtureName)
          TestBackend backendType ->
            backendType `elem` backendTypesForFixture fixtureName
          TestNoBackends -> S.null (backendTypesForFixture fixtureName)
          TestEverything -> True

    describe (show n) do
      flip aroundSomeWith (tests options) \test testEnvironment ->
        if not (n `shouldRunIn` testingMode testEnvironment)
          then pendingWith $ "Inapplicable test."
          else case skipTests options of
            Just skipMsg -> pendingWith $ "Tests skipped: " <> T.unpack skipMsg
            Nothing -> fixtureBracket fixture' test testEnvironment

-- We want to be able to report exceptions happening both during the tests
-- and at teardown, which is why we use a custom re-implementation of
-- @bracket@.
fixtureBracket :: Fixture b -> (ActionWith (TestEnvironment, b)) -> ActionWith TestEnvironment
fixtureBracket
  Fixture
    { name,
      mkLocalTestEnvironment,
      setupTeardown
    }
  actionWith
  globalTestEnvironment =
    mask \restore -> runManaged do
      -- log DB of test
      liftIO $ testLogHarness globalTestEnvironment $ "Testing " <> show name <> "..."
      localTestEnvironment <- mkLocalTestEnvironment globalTestEnvironment
      liftIO $ do
        globalTestEnvWithUnique <- setupUniqueGlobalTestEnvironment name globalTestEnvironment
        let testEnvironment = (globalTestEnvWithUnique, localTestEnvironment)

        cleanup <- runSetupActions (logger globalTestEnvWithUnique) (setupTeardown testEnvironment)

        _ <-
          catchRethrow
            (restore $ actionWith testEnvironment)
            cleanup

        -- run test-specific clean up
        cleanup

        -- drop all DBs created for the tests
        dropDatabases name globalTestEnvWithUnique

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
setupUniqueGlobalTestEnvironment :: FixtureName -> TestEnvironment -> IO TestEnvironment
setupUniqueGlobalTestEnvironment name globalTestEnvironment = do
  uniqueTestId <- nextRandom

  let globalTestEnvWithUnique =
        globalTestEnvironment
          { backendType = case name of
              Backend db -> Just db
              _ -> Nothing,
            uniqueTestId = uniqueTestId
          }

  createDatabases name globalTestEnvWithUnique
  pure globalTestEnvWithUnique

-- | A function that makes it easy to perform setup and teardown when
-- debugging/developing tests within a repl.
fixtureRepl ::
  Fixture a ->
  TestEnvironment ->
  IO (IO ())
fixtureRepl Fixture {name, mkLocalTestEnvironment, setupTeardown} globalTestEnvironment = do
  with (mkLocalTestEnvironment globalTestEnvironment) \localTestEnvironment -> do
    globalTestEnvWithUnique <- setupUniqueGlobalTestEnvironment name globalTestEnvironment

    let testEnvironment = (globalTestEnvWithUnique, localTestEnvironment)
    cleanup <- runSetupActions (logger globalTestEnvWithUnique) (setupTeardown testEnvironment)
    return cleanup

-- | Run a list of SetupActions.
--
-- * If all setup steps complete, return an IO action that runs the teardown actions in reverse order.
-- * If a setup step fails, the steps that were executed are torn down in reverse order.
-- * Teardown always collects all the exceptions that are thrown.
runSetupActions :: Logger -> [SetupAction] -> IO (IO ())
runSetupActions logger acts = go acts []
  where
    log = runLogger logger . LogHarness . T.pack

    go :: [SetupAction] -> [IO ()] -> IO (IO ())
    go actions cleanupAcc = case actions of
      [] -> return (rethrowAll cleanupAcc)
      SetupAction {setupAction, teardownAction} : rest -> do
        a <- try setupAction
        -- It would be nice to be able to log the execution of setup actions
        -- into a logfile or similar.  Using `putStrLn` interferes with the
        -- default Hspec test runner's output, so the lines below have been left
        -- commented out.
        case a of
          Left (exn :: SomeException) -> do
            log $ "Setup failed for step " ++ show (length cleanupAcc) ++ "."
            rethrowAll
              ( throwIO exn
                  : ( log ("Teardown failed for step " ++ show (length cleanupAcc) ++ ".")
                        >> teardownAction Nothing
                    )
                  : cleanupAcc
              )
            return (return ())
          Right x -> do
            log $ "Setup for step " ++ show (length cleanupAcc) ++ " succeeded."
            go
              rest
              ( ( log ("Teardown for step " ++ show (length cleanupAcc) ++ " succeeded.")
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
    -- e.g. @Postgres@ or @MySQL@
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

-- | A name describing the given context.
data FixtureName
  = Backend BackendType
  | RemoteGraphQLServer
  | Combine FixtureName FixtureName

backendTypesForFixture :: FixtureName -> S.Set BackendType
backendTypesForFixture (Backend be) = S.singleton be
backendTypesForFixture RemoteGraphQLServer = mempty
backendTypesForFixture (Combine a b) =
  backendTypesForFixture a <> backendTypesForFixture b

instance Show FixtureName where
  show (Backend backend) = show backend
  show RemoteGraphQLServer = "RemoteGraphQLServer"
  show (Combine name1 name2) = show name1 ++ "-" ++ show name2

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
combineFixtures :: LHSFixture -> RHSFixture -> Fixture (Maybe Server)
combineFixtures lhs (tableName, rhs) =
  (fixture $ Combine lhsName rhsName)
    { mkLocalTestEnvironment = lhsMkLocalTestEnvironment,
      setupTeardown = \(testEnvironment, localTestEnvironment) ->
        [SetupAction.clearMetadata testEnvironment]
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
