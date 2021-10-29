module Hasura.GraphQL.NamespaceSpec (spec) where

import Data.HashMap.Strict.InsOrd qualified as OMap
import Hasura.Generator ()
import Hasura.GraphQL.Namespace
import Hasura.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "NamespacedField" $ do
    prop "flatten/unflatten roundtrip" $ \(unflattened :: NamespacedFieldMap Int) ->
      -- If all namespaced fields are non-empty then flattening then unflattening should be the identity
      let nonEmptyFields = OMap.filter (namespacedField (const True) $ not . OMap.null) unflattened
       in unflattenNamespaces (flattenNamespaces nonEmptyFields) `shouldBe` nonEmptyFields
