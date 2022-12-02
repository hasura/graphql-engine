-- This module references specs that have been ported from pytest.
module Test.Schema.TableRelationships.PortedSpec (spec) where

import Harness.TestEnvironment
import Test.Hspec
import Test.PortedFromPytest.TestGraphQLQueryBasicCitus qualified

spec :: SpecWith GlobalTestEnvironment
spec = do
  describe "TestGraphQLQueryBasicCitus" Test.PortedFromPytest.TestGraphQLQueryBasicCitus.spec
