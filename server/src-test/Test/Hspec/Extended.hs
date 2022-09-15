-- | This module contains useful generic extensions to the Hspec testing framework.
--
-- Note: that, at the time of this writing this module has a namesake in the
-- 'tests-hspec' test suite. We might consider merging the two.
module Test.Hspec.Extended
  ( dependentSpec,
    dependentSpecWith,
  )
where

import Test.Hspec
import Prelude

-- | Mark specs as pending depending on some value.
--
-- Using this function to build specs results in clearer test results when a set
-- of tests can only conceivably succeed if some earlier test also succeeded.
dependentSpecWith ::
  forall a b c.
  (HasCallStack) =>
  (a -> b -> c) ->
  Maybe a ->
  SpecWith c ->
  SpecWith b
dependentSpecWith inject (Just a) specs = aroundWith (\spec b -> spec (inject a b)) specs
dependentSpecWith _ Nothing specs = aroundWith (\_ _ -> pendingWith "Depends on the success of a previous test") specs

-- | Mark specs as pending depending on some value.
--
-- Using this function to build specs results in clearer test results when a set
-- of tests can only conceivably succeed if some earlier test also succeeded.
--
-- This is a simplified version where only the dependent specs take arguments.
dependentSpec ::
  forall a.
  (HasCallStack) =>
  Maybe a ->
  SpecWith a ->
  Spec
dependentSpec = dependentSpecWith const
