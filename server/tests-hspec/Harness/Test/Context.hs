-- | Helper functions for testing in specific contexts.
--
--   A 'Context' represents the prerequisites for running a test,
--   such as the required backend, the setup process of tables and permissions,
--   the creation of local testEnvironment and the teardown of created context after the test
--   is done.
module Harness.Test.Context
  ( run,
    runWithLocalTestEnvironment,
    Context (..),
    context,
    ContextName (..),
    BackendType (..),
    defaultSource,
    defaultBackendTypeString,
    schemaKeyword,
    noLocalTestEnvironment,
    Options (..),
    combineOptions,
    defaultOptions,
    contextRepl,
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

--------------------------------------------------------------------------------
-- Context

-- | Runs the given tests, for each provided 'Context'@ ()@.
--
-- Each 'Context' provides distinct setup and teardown functions; 'run'
-- guarantees that the associated 'teardown' function is always called after a
-- setup, even if the tests fail.
--
-- See 'Context' for details.
--
-- This function restricts the local testEnvironment parameter for 'Context' to be '()',
-- indicating that there should be _no_ local testEnvironment.
--
-- For a more general version that can run tests for any 'Context'@ a@, see
-- 'runWithLocalTestEnvironment'.
run :: NonEmpty (Context ()) -> (Options -> SpecWith TestEnvironment) -> SpecWith TestEnvironment
run contexts tests = do
  let mappedTests opts =
        mapSpecItem
          actionWithTestEnvironmentMapping
          (mapItemAction actionWithTestEnvironmentMapping)
          (tests opts)
  runWithLocalTestEnvironment contexts mappedTests

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

-- | Runs the given tests, for each provided 'Context'@ a@.
--
-- Each 'Context' provides distinct setup and teardown functions; 'runWithLocalTestEnvironment'
-- guarantees that the associated 'teardown' function is always called after a
-- setup, even if the tests fail.
--
-- 'Context's are parameterized by the type of local testEnvironment that needs to be
-- carried throughout the tests.
--
-- See 'Context' for details.
runWithLocalTestEnvironment ::
  forall a.
  NonEmpty (Context a) ->
  (Options -> SpecWith (TestEnvironment, a)) ->
  SpecWith TestEnvironment
runWithLocalTestEnvironment contexts tests =
  for_ contexts \c -> do
    let n = name c
        co = customOptions c
        options = fromMaybe defaultOptions co
    describe (show n) $ aroundAllWith (contextBracket c) (tests options)

-- We want to be able to report exceptions happening both during the tests
-- and at teardown, which is why we use a custom re-implementation of
-- @bracket@.
contextBracket ::
  Context a ->
  ((TestEnvironment, a) -> IO ()) ->
  TestEnvironment ->
  IO ()
contextBracket Context {name, mkLocalTestEnvironment, setup, teardown} actionWith globalTestEnvironment =
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

    _ <-
      catchRethrow
        (setup testEnvironment)
        (teardown testEnvironment)

    -- Run tests.
    _ <-
      catchRethrow
        (restore $ actionWith testEnvironment)
        (teardown testEnvironment)

    -- If no exception occurred, run the normal teardown function.
    teardown testEnvironment

-- | A function that makes it easy to perform setup and teardown when
-- debugging/developing tests within a repl.
contextRepl ::
  Context a ->
  TestEnvironment ->
  IO (IO ())
contextRepl Context {mkLocalTestEnvironment, setup, teardown} globalTestEnvironment = do
  localTestEnvironment <- mkLocalTestEnvironment globalTestEnvironment
  let testEnvironment = (globalTestEnvironment, localTestEnvironment)

  catchRethrow
    (setup testEnvironment)
    (teardown testEnvironment)

  -- If no exception occurred, run the normal teardown function.
  return (teardown testEnvironment)

--------------------------------------------------------------------------------

-- | A context in which a set of tests should be executed; this could be an
-- individual backend, or a setup of several of them to test relationships.
--
-- The @a@ parameter defines the local testEnvironment, in addition to the global testEnvironment.
--
-- A test that doesn't require additional local testEnvironment can indicate this with
-- '()'.
--
-- For example a value of type @Context ()@ will have the following record
-- fields:
--
-- @
--   setup    :: TestEnvironment -> IO ()
--   teardown :: (TestEnvironment, ()) -> IO ()
--   tests    :: SpecWith (TestEnvironment, ())
-- @
--
-- However, if a test needs some custom testEnvironment it must be passed in as a tuple.
--
-- For example a value of type @Context Server@ will have the following record
-- fields:
--
-- @
--   setup    :: TestEnvironment -> IO ()
--   teardown :: (TestEnvironment, Server) -> IO ()
--   tests    :: SpecWith (TestEnvironment, Server)
-- @
data Context a = Context
  { -- | A name describing the given context.
    --
    -- e.g. @Postgres@ or @MySQL@
    name :: ContextName,
    -- | Setup actions associated with creating a local testEnvironment for this 'Context'; for example:
    --  * starting remote servers
    --
    -- If any of those resources need to be threaded throughout the tests
    -- themselves they should be returned here. Otherwise, a ()
    mkLocalTestEnvironment :: TestEnvironment -> IO a,
    -- | Setup actions associated with this 'Context'; for example:
    --  * running SQL commands
    --  * sending metadata commands
    --
    -- Takes the global 'TestEnvironment' and any local testEnvironment (i.e. @a@) as arguments.
    setup :: (TestEnvironment, a) -> IO (),
    -- | Cleanup actions associated with this 'Context'.
    --
    -- This function /must/ return any resources created or modified as part of
    -- 'setup' to their /original testEnvironment/ (whatever that may be).
    --
    -- Takes the global 'TestEnvironment' and any local testEnvironment (i.e. @a@) as arguments.
    teardown :: (TestEnvironment, a) -> IO (),
    -- | Options which modify the behavior of a given testing 'Context'; when
    -- this field is 'Nothing', tests are given the 'defaultOptions'.
    customOptions :: Maybe Options
  }

-- | A simple smart constructor for a 'Context'.
context :: ContextName -> Context ()
context name = Context {..}
  where
    mkLocalTestEnvironment = noLocalTestEnvironment
    setup = const (pure ())
    teardown = const (pure ())
    customOptions = Nothing

-- | A name describing the given context.
data ContextName
  = Backend BackendType
  | RemoteGraphQLServer
  | Combine ContextName ContextName

instance Show ContextName where
  show (Backend backend) = show backend
  show RemoteGraphQLServer = "RemoteGraphQLServer"
  show (Combine name1 name2) = show name1 ++ "-" ++ show name2

-- | Default function for 'mkLocalTestEnvironment' when there's no local testEnvironment.
noLocalTestEnvironment :: TestEnvironment -> IO ()
noLocalTestEnvironment _ = pure ()
