{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoOverloadedStrings #-}

-- | A suite of test cases for memoization monads.
-- Designed to be reusable.
module Control.Monad.MemoizationSpecDefinition (Memoizer (..), memoizationSpec) where

import Control.Monad.TimeLimit
import Data.HashMap.Strict qualified as HashMap
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Hasura.Prelude
import Language.Haskell.TH (Name)
import Test.Hspec

class
  ( forall k v. MonadTrans (m k v),
    forall k v n. (Monad n) => Functor (m k v n),
    forall k v n. (Monad n) => Applicative (m k v n),
    forall k v n. (Monad n) => Monad (m k v n),
    forall k v n s. (MonadState s n) => MonadState s (m k v n)
  ) =>
  Memoizer (m :: Type -> Type -> (Type -> Type) -> Type -> Type)
  where
  runMemoizer ::
    forall k v n a.
    (MonadFix n, MonadIO n, Eq k, Ord k, Hashable k) =>
    m k v n a ->
    n a
  memoize ::
    forall k v n.
    (MonadFix n, MonadIO n, Eq k, Ord k, Hashable k, Typeable k, Typeable v) =>
    Name ->
    k ->
    m k v n v ->
    m k v n v

memoizationSpec :: forall m. (Memoizer m) => Spec
memoizationSpec = do
  describe "circular graphs" $ checkCircularGraphs @m
  describe "infinite lists" $ checkInfiniteLists @m
  describe "memoization" $ checkMemoization @m
  describe "does not protect against bad code" $ checkFailure @m

--------------------------------------------------------------------------------
-- Graphs

data Node = Node String [Node]

nodeName :: Node -> String
nodeName (Node n _) = n

instance Show Node where
  show (Node name succs) = "Node " ++ show name ++ " " ++ show (nodeName <$> succs)

instance Eq Node where
  Node n1 s1 == Node n2 s2 = n1 == n2 && map nodeName s1 == map nodeName s2

checkCircularGraphs :: forall m. (Memoizer m) => Spec
checkCircularGraphs = do
  it "builds A -> B -> C -> A" do
    (a, b, c) <- succeedsWithinTimeLimit $ runMemoizer @m do
      let buildA = memoize 'checkCircularGraphs "a" do
            b <- buildB
            pure $ Node "a" [b]
          buildB = memoize 'checkCircularGraphs "b" do
            c <- buildC
            pure $ Node "b" [c]
          buildC = memoize 'checkCircularGraphs "c" do
            a <- buildA
            pure $ Node "c" [a]
      (,,)
        <$> buildA
        <*> buildB
        <*> buildC
    a `shouldBe` Node "a" [b]
    b `shouldBe` Node "b" [c]
    c `shouldBe` Node "c" [a]
  it "builds A -> A" do
    a <- succeedsWithinTimeLimit $ runMemoizer @m do
      let buildA = memoize 'checkCircularGraphs "a" do
            a <- buildA
            pure $ Node "a" [a]
      buildA
    a `shouldBe` Node "a" [a]

--------------------------------------------------------------------------------
-- Infinite lists

checkInfiniteLists :: forall m. (Memoizer m) => Spec
checkInfiniteLists = do
  it "builds `x = 1 : x`" do
    l <- succeedsWithinTimeLimit $ runMemoizer @m do
      let x = memoize 'checkInfiniteLists () do
            y <- x
            pure $ (1 :: Int) : y
      x
    take 5 l `shouldBe` [1, 1, 1, 1, 1]
  it "builds `[0,1,2..]`" do
    l <- succeedsWithinTimeLimit $ runMemoizer @m do
      let x = memoize 'checkInfiniteLists () do
            y <- x
            pure $ (0 :: Int) : map succ y
      x
    take 5 l `shouldBe` [0, 1, 2, 3, 4]

--------------------------------------------------------------------------------
-- Memoization

checkMemoization :: forall m. (Memoizer m) => Spec
checkMemoization = do
  it "memoizes fibo" do
    (fibos, count) <- succeedsWithinTimeLimit
      $ flip runStateT (mempty :: HashMap Int Int)
      $ runMemoizer @m do
        let fibo n = memoize 'checkMemoization n do
              modify $ HashMap.insertWith (+) n (1 :: Int)
              case n of
                0 -> pure 0
                1 -> pure 1
                _ -> (+) <$> fibo (n - 2) <*> fibo (n - 1)
        traverse fibo [0 .. 20]
    fibos !! 20 `shouldBe` (6765 :: Int)
    count `shouldBe` HashMap.fromList (zip [0 .. 20] (repeat 1))

--------------------------------------------------------------------------------
-- Failure

checkFailure :: forall m. (Memoizer m) => Spec
checkFailure = do
  it "unsuccessfully attempts to memoize Maybe" do
    result <- runWithTimeLimit $ runMemoizer @m do
      let buildA :: m String (Maybe Node) IO (Maybe Node)
          buildA = memoize 'checkFailure "a" do
            mb <- buildB
            -- a can only exist if b exists
            pure $ mb <&> \b -> Node "a" [b]
          buildB :: m String (Maybe Node) IO (Maybe Node)
          buildB = memoize 'checkFailure "b" do
            ma <- buildA
            -- b can only exist if a exists
            pure $ ma <&> \a -> Node "b" [a]
      buildA
    result `shouldBe` Nothing
  it "unsuccessfully attempts to build a self-referential int" do
    result <- runWithTimeLimit $ runMemoizer @m do
      let go = memoize 'checkFailure () do
            x <- go
            pure $ if odd x then 1 else 0 :: Int
      go
    result `shouldBe` Nothing
