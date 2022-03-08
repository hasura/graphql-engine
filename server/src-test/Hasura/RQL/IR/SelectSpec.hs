module Hasura.RQL.IR.SelectSpec (spec) where

import Data.Bifoldable
import Data.List.Extended (singleton)
import Hasura.Prelude
import Hasura.RQL.IR.Postgres (genAnnSelectG)
import Hasura.RQL.IR.Select (AnnSelectG (..), bifoldMapAnnSelectG)
import Hasura.RQL.Types
import Hedgehog
import Hedgehog.Gen (int)
import Hedgehog.Range (linear)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

newtype MyPair (b :: BackendType) r v = MyPair (r, v)
  deriving stock (Show)
  deriving newtype (Foldable, Bifoldable)

genMyPair :: MonadGen m => m r -> m v -> m (MyPair b r v)
genMyPair genR genV = do
  r <- genR
  v <- genV
  pure $ MyPair (r, v)

smallRange :: Integral a => Range a
smallRange = linear 0 10

spec :: Spec
spec = do
  describe "bifoldMapAnnSelectG" $ do
    it "bifoldMapAnnSelectG (const mempty) == foldMap" $
      hedgehog $ do
        annSelectG :: AnnSelectG ('Postgres 'Vanilla) (MyPair ('Postgres 'Vanilla) Int) Int <-
          forAll $
            genAnnSelectG
              smallRange
              (int smallRange)
              (genMyPair (int smallRange) (int smallRange))
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
              smallRange
        bifoldMapAnnSelectG (const []) singleton annSelectG === foldMap singleton annSelectG
        bifoldMapAnnSelectG singleton (const []) annSelectG === foldMap (foldMap $ bifoldMap singleton (const [])) (_asnFields annSelectG)
