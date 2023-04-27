module Data.TrieSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Trie qualified as T
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Test.Hspec
import Test.Hspec.QuickCheck

type TestTrie = T.Trie String [Int]

spec :: Spec
spec = describe "Trie" $ do
  describe "insert" $ do
    prop "insert a path into an empty trie" $ \p v -> do
      let expected = T.singleton p v :: TestTrie
      T.insert p v T.empty `shouldBe` expected

    prop "insert a singleton path into an empty trie" $ \pc v -> do
      let expected = T.Trie (HashMap.singleton pc (T.Trie mempty (Just v))) Nothing :: TestTrie
      T.insert [pc] v T.empty `shouldBe` expected

    prop "insertion is idempotent" $ \p v (t :: TestTrie) -> do
      T.insert p v (T.insert p v t) `shouldBe` T.insert p v t

    prop "insertion with mappend monoid equivalence" $ \p v (t :: TestTrie) -> do
      T.insertWith (<>) p v t `shouldBe` T.singleton p v <> t
