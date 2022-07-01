module Control.Monad.CircularSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Extended (sleep)
import Control.Monad.Circular
import Data.HashMap.Strict qualified as Map
import Data.List (nub)
import Hasura.Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "circular graphs" checkCircularGraphs
  describe "infinite lists" checkInfiniteLists
  describe "memoization" checkMemoization
  describe "does not protect against bad code" checkFailure

--------------------------------------------------------------------------------
-- Utils

runWithTimeLimit :: MonadIO m => IO a -> m (Maybe a)
runWithTimeLimit action = liftIO do
  var <- newEmptyMVar
  threadId <- forkIO do
    value <- action
    putMVar var $! value
  result <-
    foldr1 continueOnFail $ replicate 10 do
      sleep 0.01
      tryTakeMVar var
  killThread threadId
  pure result
  where
    continueOnFail step nextStep =
      step >>= \case
        Nothing -> nextStep
        Just res -> pure (Just res)

succeedsWithinTimeLimit :: (MonadIO m, MonadFail m) => IO a -> m a
succeedsWithinTimeLimit action =
  runWithTimeLimit action
    `onNothingM` fail "failed to compute in reasonable time"

--------------------------------------------------------------------------------
-- Graphs

data Node = Node String [Node]

nodeName :: Node -> String
nodeName (Node n _) = n

instance Show Node where
  show (Node name succs) = "Node " ++ show name ++ " " ++ show (nodeName <$> succs)

instance Eq Node where
  Node n1 s1 == Node n2 s2 = n1 == n2 && map nodeName s1 == map nodeName s2

checkCircularGraphs :: Spec
checkCircularGraphs = do
  it "builds A -> B -> C -> A" do
    (a, b, c) <- succeedsWithinTimeLimit $ runCircularT @String do
      let buildA = withCircular "a" do
            b <- buildB
            pure $ Node "a" [b]
          buildB = withCircular "b" do
            c <- buildC
            pure $ Node "b" [c]
          buildC = withCircular "c" do
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
    a <- succeedsWithinTimeLimit $ runCircularT @String do
      let buildA = withCircular "a" do
            a <- buildA
            pure $ Node "a" [a]
      buildA
    a `shouldBe` Node "a" [a]

--------------------------------------------------------------------------------
-- Infinite lists

checkInfiniteLists :: Spec
checkInfiniteLists = do
  it "builds `x = 1 : x`" do
    l <- succeedsWithinTimeLimit $ runCircularT do
      let x = withCircular () do
            y <- x
            pure $ (1 :: Int) : y
      x
    take 5 l `shouldBe` [1, 1, 1, 1, 1]
  it "builds `[0,1,2..]`" do
    l <- succeedsWithinTimeLimit $ runCircularT do
      let x = withCircular () do
            y <- x
            pure $ (0 :: Int) : map succ y
      x
    take 5 l `shouldBe` [0, 1, 2, 3, 4]

--------------------------------------------------------------------------------
-- Memoization

checkMemoization :: Spec
checkMemoization = do
  it "memoizes fibo" do
    (fibos, count) <- succeedsWithinTimeLimit $
      flip runStateT mempty $ runCircularT @Int do
        let fibo n = withCircular n do
              modify $ Map.insertWith (+) n 1
              case n of
                0 -> pure 0
                1 -> pure 1
                _ -> (+) <$> fibo (n - 2) <*> fibo (n - 1)
        traverse fibo [0 .. 20]
    fibos !! 20 `shouldBe` (6765 :: Int)
    nub (Map.elems count) `shouldBe` [1 :: Int]

--------------------------------------------------------------------------------
-- Failure

checkFailure :: Spec
checkFailure = do
  it "unsuccessfully attempts to memoize Maybe" do
    result <- runWithTimeLimit $ runCircularT do
      let buildA = withCircular "a" do
            mb <- buildB
            -- a can only exist if b exists
            pure $ mb <&> \b -> Node "a" [b]
          buildB = withCircular "b" do
            ma <- buildA
            -- b can only exist if a exists
            pure $ ma <&> \a -> Node "b" [a]
      buildA :: CircularT String (Maybe Node) IO (Maybe Node)
    result `shouldBe` Nothing
  it "unsuccessfully attempts to build a self-referential int" do
    result <- runWithTimeLimit $ runCircularT do
      let go = withCircular () do
            x <- go
            pure $ if odd x then 1 else 0 :: Int
      go
    result `shouldBe` Nothing
