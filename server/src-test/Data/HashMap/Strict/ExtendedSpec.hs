module Data.HashMap.Strict.ExtendedSpec
  ( spec,
  )
where

import Data.HashMap.Strict.Extended qualified as Map
import Hasura.Prelude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "isInverseOf" $ do
  it "is satisfied by maps with the same unique keys as values" $
    property $
      \(xs :: [Int]) -> do
        let m = Map.fromList $ zip xs xs
        m `Map.isInverseOf` m

  it "is satisfied by maps with swapped unique keys and values" $
    property $
      \(vals :: [Int]) -> do
        let keys = show <$> vals
        let forward = Map.fromList $ zip keys vals
        let backward = Map.fromList $ zip vals keys
        forward `Map.isInverseOf` backward

  it "fails when different keys map to one value" $
    property $
      \(NonNegative (x :: Int)) (Positive (n :: Int)) -> do
        let keys = take (n + 2) [x ..]
        let vals = even <$> keys
        let forward = Map.fromList $ zip keys vals
        let backward = Map.fromList $ zip vals keys
        not $ forward `Map.isInverseOf` backward

  it "passes some trivial examples as a smoke test" $ do
    let fwd = Map.fromList @Int @Bool
    let bwd = Map.fromList @Bool @Int
    and
      [ fwd [] `Map.isInverseOf` bwd [],
        not $ fwd [(1, True)] `Map.isInverseOf` bwd [],
        not $ fwd [] `Map.isInverseOf` bwd [(True, 1)],
        fwd [(1, True)] `Map.isInverseOf` bwd [(True, 1)],
        not $ fwd [(2, True)] `Map.isInverseOf` bwd [(True, 1)]
      ]
