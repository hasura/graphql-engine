{-# LANGUAGE DeriveAnyClass #-}

-- | Helper functions for easily testing features.
module Harness.Test.Feature
  ( run,
    runWithLocalState,
    Context (..),
    Options (..),
    defaultOptions,
  )
where

import Control.Exception.Safe (Exception, SomeException, catch, mask, throwIO)
import Data.Foldable (for_)
import Harness.State (State)
import Test.Hspec (ActionWith, SpecWith, aroundAllWith, describe)
import Test.Hspec.Core.Spec (Item (..), mapSpecItem)
import Prelude

--------------------------------------------------------------------------------
-- Feature

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
-- Each 'Context' provides distinct setup and teardown functions; 'feature'
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
  for_ contexts \context@Context {name, options} ->
    describe name $ aroundAllWith (contextBracket context) (tests options)
  where
    -- We want to be able to report exceptions happening both during the tests
    -- and at teardown, which is why we use a custom re-implementation of
    -- @bracket@.
    contextBracket ::
      forall b.
      Context a ->
      ((State, a) -> IO b) ->
      State ->
      IO ()
    contextBracket Context {setup, teardown} actionWith globalState =
      mask \restore -> do
        -- Get local test state from the `setup` function.
        localState <- setup globalState
        let state = (globalState, localState)

        -- Run tests.
        _ <-
          catch
            (restore $ actionWith state)
            ( \restoreEx -> do
                -- On test error, attempt to run `teardown`...
                teardown state
                  `catch`
                  -- ...if it fails as well, bundle both exceptions together and
                  -- rethrow them...
                  (throwIO . Exceptions restoreEx)
                -- ...otherwise rethrow the original exception.
                throwIO restoreEx
            )
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
  { -- | A string describing the given context.
    --
    -- e.g. @"PostgreSQL"@ or @"MySQL v1.2"@
    name :: String,
    -- | Setup actions associated with this 'Context'; for example:
    --  * running SQL commands
    --  * starting remote servers
    --  * sending metadata commands
    --
    -- If any of those resources need to be threaded throughout the tests
    -- themselves they should be returned here.
    setup :: State -> IO a,
    -- | Cleanup actions associated with this 'Context'.
    --
    -- This function /must/ return any resources created or modified as part of
    -- 'setup' to their /original state/ (whatever that may be).
    --
    -- Takes the global 'State' and any local state (i.e. @a@) as arguments.
    teardown :: (State, a) -> IO (),
    -- | Options which modify the behavior of a given testing 'Context'.
    options :: Options
  }

data Options = Options
  { -- | Whether a given testing 'Context' should treat numeric values as
    -- strings.
    --
    -- This is primarily a workaround for tests which run BigQuery.
    stringifyNumbers :: Bool
  }

defaultOptions :: Options
defaultOptions =
  Options
    { stringifyNumbers = False
    }

--------------------------------------------------------------------------------
-- Local helpers

-- | Two exceptions, bundled as one.
data Exceptions
  = Exceptions SomeException SomeException
  deriving anyclass (Exception)

instance Show Exceptions where
  show (Exceptions e1 e2) =
    unlines
      [ "1. " <> show e1,
        "",
        "2. " <> show e2
      ]
