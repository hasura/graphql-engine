{-# LANGUAGE OverloadedLists #-}

module Hasura.RQL.Types.EndpointSpec (spec) where

import qualified Data.HashMap.Strict            as M

import           Hasura.Prelude
import           Hasura.RQL.Types.Endpoint.Trie (MatchResult (..), MultiMapTrie, Path,
                                                 PathComponent (..), Trie (..), ambiguousPaths,
                                                 insertPath, matchPath, singletonMultiMap,
                                                 singletonTrie)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                ((==>))

type T = MultiMapTrie Int String Int

e :: T
e = mempty

sing :: String -> Int -> T
sing k s = Trie M.empty $ singletonMultiMap k s

insert :: (Eq a, Hashable a, Eq k, Hashable k, Ord v) => Path a -> k -> v -> MultiMapTrie a k v -> MultiMapTrie a k v
insert p k = insertPath p . singletonMultiMap k

inserts :: (Eq a, Hashable a, Eq k, Hashable k, Ord v) => Path a -> [k] -> v -> MultiMapTrie a k v -> MultiMapTrie a k v
inserts p ks v t = foldl' (\t' k -> insert p k v t') t ks

spec :: Spec
spec = describe "Endpoint" $ do
  describe "Trie" $ do
      describe "insertPath" $ do
        prop "insert empty path into emtpy trie" $ \k s ->
            insert [] k s e `shouldBe` sing k s

        prop "insert singleton path into empty trie" $ \pc k s -> do
            let expected = Trie (M.singleton pc $ sing k s) mempty
            insert [pc] k s e `shouldBe` expected

        prop "idempotent" $ \k p s (t::T) -> do
            insert p k s (insert p k s t) `shouldBe` insert p k s t

        prop "insert monoid equivalence" $ \p v (t::T) -> do
            insertPath p v t `shouldBe` singletonTrie p v <> t

      describe "matchPath" $ do
        prop "matchPath in empty Trie returns no matches" $ \k xs ->
            matchPath k xs e `shouldBe` MatchNotFound

        prop "insert then lookup should match" $ \k xs s -> do
            let t = insert (PathLiteral <$> xs) k s e
            matchPath k xs t `shouldBe` MatchFound s []

        prop "single path param should match" $ \k x s -> do
            let t = insert [PathParam] k s e
            matchPath k [x] t `shouldBe` MatchFound s [x]

        prop "multiple path params should match" $ \k xs s -> do
            let t = insert (PathParam <$ xs) k s e
            matchPath k xs t `shouldBe` MatchFound s xs

        prop "not allowed method" $ \k k' xs s -> k /= k' ==> do
            let t = insert (PathLiteral <$> xs) k' s e
            case matchPath k xs t of
                MatchMissingKey ks -> ks `shouldBe` k' :| []
                r -> expectationFailure $ "Expected MatchMissingKey. Got " <> show r

      describe "ambiguousPaths" $ do
        let amb = map fst . ambiguousPaths

        it "empty trie" $
            amb e `shouldBe` []

        prop "param/literal at start" $ \(t::T) -> do
            let t' = inserts [PathParam] ["GET", "POST"] 42 $ inserts [PathLiteral 0] ["POST", "PUT"] 43 t
            amb t' `shouldContain` [[PathLiteral 0]]

        it "param/literal at start with diffent method" $ do
            let t' = insert [PathParam] "GET" 42 $ insert [PathLiteral 0] "POST" 43 e
            amb t' `shouldNotContain` [[PathLiteral 0]]

        prop "param/literal at end" $ \k (t::T) -> do
            let t' = insert [PathLiteral 0, PathParam] k 42 $ insert [PathLiteral 0, PathLiteral 1] k 43 t
            amb t' `shouldContain` [[PathLiteral 0, PathLiteral 1]]

        prop "param/literal in middle" $ \k (t::T) -> do
            let t' = insert [PathLiteral 0, PathParam, PathLiteral 2] k 42 $ insert [PathLiteral 0, PathLiteral 1, PathLiteral 2] k 43 t
            amb t' `shouldContain` [[PathLiteral 0, PathLiteral 1, PathLiteral 2]]

        it "different path / same method" $ do
            let t = insert [PathLiteral 0] "GET" 42 $ insert [PathLiteral 1] "GET" 43 e
            amb t `shouldBe` []

        it "same path / different method" $ do
            let t = insert [PathLiteral 0] "GET" 42 $ insert [PathLiteral 0] "POST" 43 e
            amb t `shouldBe` []

        prop "same path / same method" $ \(t::T) -> do
            let t' = insert [PathLiteral 0] "GET" 42 $ insert [PathLiteral 0] "GET" 43 t
            amb t' `shouldContain` [[PathLiteral 0]]

        prop "singleton trie" $ \k s -> do
            amb (sing k s) `shouldBe` []

        -- TODO: Add a better test

        -- prop "different literals" $ \(t0::T) t1 s -> do
        --     amb (Trie (M.fromList [(PathLiteral 0, t0), (PathLiteral 1, t1)]) s)
        --         `shouldNotContain` [[PathLiteral 0], [PathLiteral 1]]
