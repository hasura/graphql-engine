-- | Custom test fixture options.
module Harness.Test.CustomOptions
  ( Options (..),
    combineOptions,
    defaultOptions,
  )
where

import Harness.Exceptions
import Hasura.Prelude

-- | Custom test fixture options.
data Options = Options
  { -- | Whether a given testing fixture should treat numeric values as strings.
    --
    -- This is primarily a workaround for tests which run BigQuery.
    stringifyNumbers :: Bool,
    -- | Whether to skip this Fixture
    --
    -- Useful for implementing tests that we don't want to depend on in CI yet
    -- Text is reason for skipping
    skipTests :: Maybe Text
  }

-- | This function can be used to combine two sets of 'Option's when creating
-- custom composite 'Fixture's.
--
-- NOTE: This function throws an impure exception if the options are
-- irreconcilable.
combineOptions :: (HasCallStack) => Maybe Options -> Maybe Options -> Maybe Options
combineOptions (Just lhs) (Just rhs) =
  let -- 'stringifyNumbers' can only be unified if both sides have the same value.
      stringifyNumbers =
        if lhsStringify == rhsStringify
          then lhsStringify
          else reportInconsistency "stringifyNumbers" lhsStringify rhsStringify
      skipTests =
        if lhsSkip == lhsSkip
          then lhsSkip
          else reportInconsistency "skipTests" lhsSkip rhsSkip
   in Just Options {..}
  where
    reportInconsistency fieldName lhsValue rhsValue =
      error $ "Could not reconcile '" <> fieldName <> "'\n  lhs value: " <> show lhsValue <> "\n  rhs value: " <> show rhsValue
    Options {stringifyNumbers = lhsStringify, skipTests = lhsSkip} = lhs
    Options {stringifyNumbers = rhsStringify, skipTests = rhsSkip} = rhs
combineOptions mLhs mRhs = mLhs <|> mRhs

defaultOptions :: Options
defaultOptions =
  Options
    { stringifyNumbers = False,
      skipTests = Nothing
    }
