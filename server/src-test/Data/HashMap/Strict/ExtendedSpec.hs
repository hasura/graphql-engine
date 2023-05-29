module Data.HashMap.Strict.ExtendedSpec
  ( spec,
  )
where

import Data.HashMap.Strict.Extended qualified as HashMap
import Hasura.Prelude
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "isInverseOf" $ do
  it "is satisfied by maps with the same unique keys as values"
    $ property
    $ \(xs :: [Int]) -> do
      let m = HashMap.fromList $ zip xs xs
      m `HashMap.isInverseOf` m

  it "is satisfied by maps with swapped unique keys and values"
    $ property
    $ \(vals :: [Int]) -> do
      let keys = show <$> vals
      let forward = HashMap.fromList $ zip keys vals
      let backward = HashMap.fromList $ zip vals keys
      forward `HashMap.isInverseOf` backward

  it "fails when different keys map to one value"
    $ property
    $ \(NonNegative (x :: Int)) (Positive (n :: Int)) -> do
      let keys = take (n + 2) [x ..]
      let vals = even <$> keys
      let forward = HashMap.fromList $ zip keys vals
      let backward = HashMap.fromList $ zip vals keys
      not $ forward `HashMap.isInverseOf` backward

  it "passes some trivial examples as a smoke test" $ do
    let fwd = HashMap.fromList @Int @Bool
    let bwd = HashMap.fromList @Bool @Int
    and
      [ fwd [] `HashMap.isInverseOf` bwd [],
        not $ fwd [(1, True)] `HashMap.isInverseOf` bwd [],
        not $ fwd [] `HashMap.isInverseOf` bwd [(True, 1)],
        fwd [(1, True)] `HashMap.isInverseOf` bwd [(True, 1)],
        not $ fwd [(2, True)] `HashMap.isInverseOf` bwd [(True, 1)]
      ]
