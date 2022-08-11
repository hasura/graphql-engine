-- | This module defines a way to setup test fixtures which can help defining
-- tests.
--
-- Central types and functions are 'Fixture', 'SetupAction', and 'run'.
module Harness.Test.Fixture
  ( run,
    runWithLocalTestEnvironment,
    Fixture (..),
    fixture,
    FixtureName (..),
    BackendType (..),
    defaultSource,
    defaultBackendTypeString,
    noLocalTestEnvironment,
    SetupAction (..),
    Options (..),
    combineOptions,
    defaultOptions,
    fixtureRepl,
  )
where

import Data.UUID.V4 (nextRandom)
import Harness.Exceptions
import Harness.Test.BackendType
import Harness.Test.CustomOptions
import Harness.Test.Hspec.Extended
import Harness.TestEnvironment (TestEnvironment (..))
import Hasura.Prelude
import Test.Hspec (ActionWith, SpecWith, aroundAllWith, describe)
import Test.Hspec.Core.Spec (mapSpecItem)

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
run :: NonEmpty (Fixture ()) -> (Options -> SpecWith TestEnvironment) -> SpecWith TestEnvironment
run fixtures tests = do
  let mappedTests opts =
        mapSpecItem
          actionWithTestEnvironmentMapping
          (mapItemAction actionWithTestEnvironmentMapping)
          (tests opts)
  runWithLocalTestEnvironment fixtures mappedTests

-- | Observe that there is a direct correspondance (i.e. an isomorphism) from
-- @TestEnvironment@ to @(TestEnvironment, ())@ within 'ActionWith'.
--
-- NOTE: 'ActionWith'@ a@ is a type alias for @a -> IO ()@; thus, the fully
-- expanded type signature here is @(TestEnvironment -> IO ()) -> (TestEnvironment, ()) -> IO ()@.
--
-- NOTE: This can possibly be generalized to a @Control.Lens.Iso@.
--
-- NOTE: This should probably be extracted to some common helper module (e.g.
-- @Harness.TestEnvironment@).
actionWithTestEnvironmentMapping :: ActionWith TestEnvironment -> ActionWith (TestEnvironment, ())
actionWithTestEnvironmentMapping actionWith (testEnvironment, _) = actionWith testEnvironment

-- | Runs the given tests, for each provided 'Fixture'@ a@.
--
-- Each 'Fixture' provides a list of 'SetupActions';
-- 'runWithLocalTestEnvironment' guarantees that the associated 'teardown'
-- function is always called after a setup, even if the tests fail.
--
-- 'Fixture's are parameterized by the type of local testEnvironment that needs
-- to be carried throughout the tests.
--
-- See 'Fixture' for details.
runWithLocalTestEnvironment ::
  forall a.
  NonEmpty (Fixture a) ->
  (Options -> SpecWith (TestEnvironment, a)) ->
  SpecWith TestEnvironment
runWithLocalTestEnvironment fixtures tests =
  for_ fixtures \context -> do
    let n = name context
        co = customOptions context
        options = fromMaybe defaultOptions co
    describe (show n) $ aroundAllWith (fixtureBracket context) (tests options)

-- We want to be able to report exceptions happening both during the tests
-- and at teardown, which is why we use a custom re-implementation of
-- @bracket@.
fixtureBracket :: Fixture b -> ((TestEnvironment, b) -> IO a) -> TestEnvironment -> IO ()
fixtureBracket Fixture {name, mkLocalTestEnvironment, setupTeardown} actionWith globalTestEnvironment =
  mask \restore -> do
    localTestEnvironment <- mkLocalTestEnvironment globalTestEnvironment

    -- create a unique id to differentiate this set of tests
    uniqueTestId <- nextRandom

    let globalTestEnvWithUnique =
          globalTestEnvironment
            { backendType = case name of
                Backend db -> Just db
                _ -> Nothing,
              uniqueTestId = uniqueTestId
            }

    let testEnvironment = (globalTestEnvWithUnique, localTestEnvironment)

    cleanup <- runSetupActions (setupTeardown testEnvironment)

    _ <-
      catchRethrow
        (restore $ actionWith testEnvironment)
        cleanup

    cleanup

-- | A function that makes it easy to perform setup and teardown when
-- debugging/developing tests within a repl.
fixtureRepl ::
  Fixture a ->
  TestEnvironment ->
  IO (IO ())
fixtureRepl Fixture {mkLocalTestEnvironment, setupTeardown} globalTestEnvironment = do
  localTestEnvironment <- mkLocalTestEnvironment globalTestEnvironment
  let testEnvironment = (globalTestEnvironment, localTestEnvironment)

  cleanup <- runSetupActions (setupTeardown testEnvironment)
  return cleanup

-- | Run a list of SetupActions.
--
-- * If all setup steps complete, return an IO action that runs the teardown actions in reverse order.
-- * If a setup step fails, the steps that were executed are torn down in reverse order.
-- * Teardown always collects all the exceptions that are thrown.
runSetupActions :: [SetupAction] -> IO (IO ())
runSetupActions acts = go acts []
  where
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
            -- putStrLn $ "setup step " ++ show (length cleanupAcc)  ++ " failed."

            rethrowAll (throwIO exn : ({-putStrLn ("teardown the failed step " ++ show (length cleanupAcc)) >>-} teardownAction Nothing) : cleanupAcc)
            return (return ())
          Right x -> do
            -- putStrLn $ "setup step " ++ show (length cleanupAcc)  ++ " succeded."
            go rest (({-putStrLn ("teardown the successfull step " ++ show (length cleanupAcc)) >>-} teardownAction (Just x)) : cleanupAcc)

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
    -- 'Fixture'; for example:
    --
    --  * starting remote servers
    --
    -- If any of those resources need to be threaded throughout the tests
    -- themselves they should be returned here. Otherwise, a ()
    mkLocalTestEnvironment :: TestEnvironment -> IO a,
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

-- | a 'SetupAction' encodes how to setup and tear down a single piece of test
-- system state.
--
-- The value produced by a 'setupAction' is to be input into the corresponding
-- 'teardownAction', if the 'setupAction' completed without throwing an
-- exception.
data SetupAction = forall a.
  SetupAction
  { setupAction :: IO a,
    teardownAction :: Maybe a -> IO ()
  }

-- | A name describing the given context.
data FixtureName
  = Backend BackendType
  | RemoteGraphQLServer
  | Combine FixtureName FixtureName

instance Show FixtureName where
  show (Backend backend) = show backend
  show RemoteGraphQLServer = "RemoteGraphQLServer"
  show (Combine name1 name2) = show name1 ++ "-" ++ show name2

-- | Default function for 'mkLocalTestEnvironment' when there's no local testEnvironment.
noLocalTestEnvironment :: TestEnvironment -> IO ()
noLocalTestEnvironment _ = pure ()
