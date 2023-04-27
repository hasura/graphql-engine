module Hasura.GraphQL.NamespaceSpec (spec) where

import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Hasura.GraphQL.Namespace
  ( NamespacedFieldMap,
    flattenNamespaces,
    namespacedField,
    unflattenNamespaces,
  )
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
  describe "NamespacedField" $ do
    prop "flatten/unflatten roundtrip" $ \(unflattened :: NamespacedFieldMap Int) ->
      -- If all namespaced fields are non-empty then flattening then unflattening should be the identity
      let nonEmptyFields = InsOrdHashMap.filter (namespacedField (const True) $ not . InsOrdHashMap.null) unflattened
       in unflattenNamespaces (flattenNamespaces nonEmptyFields) `shouldBe` nonEmptyFields
