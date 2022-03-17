-- | Helper functions for testing in specific contexts.
--
--   A 'Context' represents the prerequisites for running a test,
--   such as the required backend, the setup process of tables and permissions,
--   the creation of local state and the teardown of created context after the test
--   is done.
module Harness.Test.Context
  ( run,
    runWithLocalState,
    Context (..),
    ContextName (..),
    BackendType (..),
    defaultSource,
    defaultBackendTypeString,
    defaultSchema,
    noLocalState,
    Options (..),
    combineOptions,
    defaultOptions,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Harness.Exceptions (catchRethrow, mask)
import Harness.State (State)
import Harness.Test.BackendType
import Test.Hspec (ActionWith, HasCallStack, SpecWith, aroundAllWith, describe)
import Test.Hspec.Core.Spec (Item (..), mapSpecItem)
import Prelude

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
-- This function restricts the local state parameter for 'Context' to be '()',
-- indicating that there should be _no_ local state.
--
-- For a more general version that can run tests for any 'Context'@ a@, see
-- 'runWithLocalState'.
run :: [Context ()] -> (Options -> SpecWith State) -> SpecWith State
run contexts tests = do
  let mappedTests opts =
        mapSpecItem
          actionWithStateMapping
          (mapItemAction actionWithStateMapping)
          (tests opts)
  runWithLocalState contexts mappedTests

-- | Observe that there is a direct correspondance (i.e. an isomorphism) from
-- @State@ to @(State, ())@ within 'ActionWith'.
--
-- NOTE: 'ActionWith'@ a@ is a type alias for @a -> IO ()@; thus, the fully
-- expanded type signature here is @(State -> IO ()) -> (State, ()) -> IO ()@.
--
-- NOTE: This can possibly be generalized to a @Control.Lens.Iso@.
--
-- NOTE: This should probably be extracted to some common helper module (e.g.
-- @Harness.State@).
actionWithStateMapping :: ActionWith State -> ActionWith (State, ())
actionWithStateMapping actionWith (state, _) = actionWith state

-- | Modify an 'Item'@ a@ by way of mapping its 'ActionWith'@ a@ function to
-- some 'ActionWith'@ b@, producing an 'Item'@ b@.
--
-- This can be useful when one wants to modify the state parameter in a
-- 'SpecWith' test tree, without having to resolve the type mismatch using some
-- combination of type families and helper type classes.
--
-- NOTE: This should go in some sort of @Test.Hspec.Core.Spec.Extended@ module.
mapItemAction :: (ActionWith a -> ActionWith b) -> Item a -> Item b
mapItemAction mapActionWith item@Item {itemExample} =
  let mappedExample params next callback =
        itemExample
          params
          (next . mapActionWith)
          callback
   in item {itemExample = mappedExample}

-- | Runs the given tests, for each provided 'Context'@ a@.
--
-- Each 'Context' provides distinct setup and teardown functions; 'runWithLocalState'
-- guarantees that the associated 'teardown' function is always called after a
-- setup, even if the tests fail.
--
-- 'Context's are parameterized by the type of local state that needs to be
-- carried throughout the tests.
--
-- See 'Context' for details.
runWithLocalState ::
  forall a.
  [Context a] ->
  (Options -> SpecWith (State, a)) ->
  SpecWith State
runWithLocalState contexts tests =
  for_ contexts \context@Context {name, customOptions} -> do
    let options = fromMaybe defaultOptions customOptions
    describe (show name) $ aroundAllWith (contextBracket context) (tests options)
  where
    -- We want to be able to report exceptions happening both during the tests
    -- and at teardown, which is why we use a custom re-implementation of
    -- @bracket@.
    contextBracket ::
      Context a ->
      ((State, a) -> IO ()) ->
      State ->
      IO ()
    contextBracket Context {mkLocalState, setup, teardown} actionWith globalState =
      mask \restore -> do
        localState <- mkLocalState globalState
        let state = (globalState, localState)

        catchRethrow
          (setup state)
          (teardown state)

        -- Run tests.
        _ <-
          catchRethrow
            (restore $ actionWith state)
            (teardown state)
        -- If no exception occurred, run the normal teardown function.
        teardown state

--------------------------------------------------------------------------------

-- | A context in which a set of tests should be executed; this could be an
-- individual backend, or a setup of several of them to test relationships.
--
-- The @a@ parameter defines the local state, in addition to the global state.
--
-- A test that doesn't require additional local state can indicate this with
-- '()'.
--
-- For example a value of type @Context ()@ will have the following record
-- fields:
--
-- @
--   setup    :: State -> IO ()
--   teardown :: (State, ()) -> IO ()
--   tests    :: SpecWith (State, ())
-- @
--
-- However, if a test needs some custom state it must be passed in as a tuple.
--
-- For example a value of type @Context Server@ will have the following record
-- fields:
--
-- @
--   setup    :: State -> IO ()
--   teardown :: (State, Server) -> IO ()
--   tests    :: SpecWith (State, Server)
-- @
data Context a = Context
  { -- | A name describing the given context.
    --
    -- e.g. @Postgres@ or @MySQL@
    name :: ContextName,
    -- | Setup actions associated with creating a local state for this 'Context'; for example:
    --  * starting remote servers
    --
    -- If any of those resources need to be threaded throughout the tests
    -- themselves they should be returned here. Otherwise, a ()
    mkLocalState :: State -> IO a,
    -- | Setup actions associated with this 'Context'; for example:
    --  * running SQL commands
    --  * sending metadata commands
    --
    -- Takes the global 'State' and any local state (i.e. @a@) as arguments.
    setup :: (State, a) -> IO (),
    -- | Cleanup actions associated with this 'Context'.
    --
    -- This function /must/ return any resources created or modified as part of
    -- 'setup' to their /original state/ (whatever that may be).
    --
    -- Takes the global 'State' and any local state (i.e. @a@) as arguments.
    teardown :: (State, a) -> IO (),
    -- | Options which modify the behavior of a given testing 'Context'; when
    -- this field is 'Nothing', tests are given the 'defaultOptions'.
    customOptions :: Maybe Options
  }

-- | A name describing the given context.
data ContextName
  = Backend BackendType
  | Combine ContextName ContextName

instance Show ContextName where
  show (Backend backend) = show backend
  show (Combine name1 name2) = show name1 ++ "-" ++ show name2

-- | Default function for 'mkLocalState' when there's no local state.
noLocalState :: State -> IO ()
noLocalState _ = pure ()

data Options = Options
  { -- | Whether a given testing 'Context' should treat numeric values as
    -- strings.
    --
    -- This is primarily a workaround for tests which run BigQuery.
    stringifyNumbers :: Bool
  }

-- | This function can be used to combine two sets of 'Option's when creating
-- custom composite 'Context's.
--
-- NOTE: This function throws an impure exception if the options are
-- irreconcilable.
combineOptions :: HasCallStack => Maybe Options -> Maybe Options -> Maybe Options
combineOptions (Just lhs) (Just rhs) =
  let -- 'stringifyNumbers' can only be unified if both sides have the same value.
      stringifyNumbers =
        if lhsStringify == rhsStringify
          then lhsStringify
          else reportInconsistency "stringifyNumbers" lhsStringify rhsStringify
   in Just Options {..}
  where
    reportInconsistency fieldName lhsValue rhsValue =
      error $ "Could not reconcile '" <> fieldName <> "'\n  lhs value: " <> show lhsValue <> "\n  rhs value: " <> show rhsValue
    Options {stringifyNumbers = lhsStringify} = lhs
    Options {stringifyNumbers = rhsStringify} = rhs
combineOptions mLhs mRhs = mLhs <|> mRhs

defaultOptions :: Options
defaultOptions =
  Options
    { stringifyNumbers = False
    }
