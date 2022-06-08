-- | This module contains auxiliary extentions to hspec
module Harness.Test.Hspec.Extended
  ( mapItemAction,
  )
where

import Test.Hspec
import Test.Hspec.Core.Spec
import Prelude

-- | Modify an 'Item'@ a@ by way of mapping its 'ActionWith'@ a@ function to
-- some 'ActionWith'@ b@, producing an 'Item'@ b@.
--
-- This can be useful when one wants to modify the testEnvironment parameter in a
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
