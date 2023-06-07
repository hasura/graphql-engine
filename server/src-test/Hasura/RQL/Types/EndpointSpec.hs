module Hasura.RQL.Types.EndpointSpec (spec) where

import Data.HashMap.Strict.Multi qualified as MMap
import Data.Trie qualified as Trie
import Hasura.Prelude
import Hasura.QuickCheck.Instances ()
import Hasura.RQL.Types.Endpoint.Trie
  ( MatchResult (..),
    MultiMapPathTrie,
    PathComponent (..),
    ambiguousPaths,
    matchPath,
  )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))

type TestTrie = MultiMapPathTrie Int String Int

emptyTrie :: TestTrie
emptyTrie = Trie.empty

singleton :: String -> Int -> TestTrie
singleton k s = Trie.singleton [] $ MMap.singleton k s

insert :: (Hashable a, Hashable k, Ord v) => [PathComponent a] -> k -> v -> MultiMapPathTrie a k v -> MultiMapPathTrie a k v
insert p k = Trie.insertWith (<>) p . MMap.singleton k

inserts :: (Hashable a, Hashable k, Ord v) => [PathComponent a] -> [k] -> v -> MultiMapPathTrie a k v -> MultiMapPathTrie a k v
inserts p ks v t = foldl' (\t' k -> insert p k v t') t ks

spec :: Spec
spec = describe "Endpoint" $ do
  describe "Trie" $ do
    describe "matchPath" $ do
      prop "matchPath in empty Trie returns no matches" $ \k xs ->
        matchPath k xs emptyTrie `shouldBe` MatchNotFound

      prop "insert then lookup should match" $ \k xs s -> do
        let t = insert (PathLiteral <$> xs) k s emptyTrie
        matchPath k xs t `shouldBe` MatchFound s []

      prop "single path param should match" $ \k x s -> do
        let t = insert [PathParam] k s emptyTrie
        matchPath k [x] t `shouldBe` MatchFound s [x]

      prop "multiple path params should match" $ \k xs s -> do
        let t = insert (PathParam <$ xs) k s emptyTrie
        matchPath k xs t `shouldBe` MatchFound s xs

      prop "not allowed method" $ \k k' xs s ->
        k /= k' ==> do
          let t = insert (PathLiteral <$> xs) k' s emptyTrie
          case matchPath k xs t of
            MatchMissingKey ks -> ks `shouldBe` k' :| []
            r -> expectationFailure $ "Expected MatchMissingKey. Got " <> show r

    describe "ambiguousPaths" $ do
      let amb = map fst . ambiguousPaths

      it "empty trie"
        $ amb emptyTrie
        `shouldBe` []

      prop "param/literal at start" $ \(t :: TestTrie) -> do
        let t' = inserts [PathParam] ["GET", "POST"] 42 $ inserts [PathLiteral 0] ["POST", "PUT"] 43 t
        amb t' `shouldContain` [[PathLiteral 0]]

      it "param/literal at start with diffent method" $ do
        let t' = insert [PathParam] "GET" 42 $ insert [PathLiteral 0] "POST" 43 emptyTrie
        amb t' `shouldNotContain` [[PathLiteral 0]]

      prop "param/literal at end" $ \k (t :: TestTrie) -> do
        let t' = insert [PathLiteral 0, PathParam] k 42 $ insert [PathLiteral 0, PathLiteral 1] k 43 t
        amb t' `shouldContain` [[PathLiteral 0, PathLiteral 1]]

      prop "param/literal in middle" $ \k (t :: TestTrie) -> do
        let t' = insert [PathLiteral 0, PathParam, PathLiteral 2] k 42 $ insert [PathLiteral 0, PathLiteral 1, PathLiteral 2] k 43 t
        amb t' `shouldContain` [[PathLiteral 0, PathLiteral 1, PathLiteral 2]]

      it "different path / same method" $ do
        let t = insert [PathLiteral 0] "GET" 42 $ insert [PathLiteral 1] "GET" 43 emptyTrie
        amb t `shouldBe` []

      it "same path / different method" $ do
        let t = insert [PathLiteral 0] "GET" 42 $ insert [PathLiteral 0] "POST" 43 emptyTrie
        amb t `shouldBe` []

      prop "same path / same method" $ \(t :: TestTrie) -> do
        let t' = insert [PathLiteral 0] "GET" 42 $ insert [PathLiteral 0] "GET" 43 t
        amb t' `shouldContain` [[PathLiteral 0]]

      prop "singleton trie" $ \k s -> do
        amb (singleton k s) `shouldBe` []

-- TODO: Add a better test

-- prop "different literals" $ \(t0::T) t1 s -> do
--     amb (Trie (M.fromList [(PathLiteral 0, t0), (PathLiteral 1, t1)]) s)
--         `shouldNotContain` [[PathLiteral 0], [PathLiteral 1]]
