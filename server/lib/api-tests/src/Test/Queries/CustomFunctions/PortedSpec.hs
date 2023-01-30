-- This module references specs that have been ported from pytest.
module Test.Queries.CustomFunctions.PortedSpec (spec) where

import Harness.TestEnvironment
import Test.Hspec
import Test.PortedFromPytest.TestGraphQLQueryFunctions qualified

spec :: SpecWith GlobalTestEnvironment
spec = do
  describe "TestGraphQLQueryFunctions" Test.PortedFromPytest.TestGraphQLQueryFunctions.spec
