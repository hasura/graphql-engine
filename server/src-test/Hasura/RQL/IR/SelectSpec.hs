module Hasura.RQL.IR.SelectSpec (spec) where

import Data.Bifoldable
import Hasura.Backends.Postgres.RQLGenerator
import Hasura.Generator.Common (defaultRange)
import Hasura.Prelude
import Hasura.RQL.IR.Select (AnnSelectG (..), bifoldMapAnnSelectG)
import Hasura.RQL.Types.BackendType
import Hedgehog
import Hedgehog.Gen (int)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

newtype MyPair (b :: BackendType) r v = MyPair (r, v)
  deriving stock (Show)
  deriving newtype (Foldable, Bifoldable)

genMyPair :: (MonadGen m) => m r -> m v -> m (MyPair b r v)
genMyPair genR genV = do
  r <- genR
  v <- genV
  pure $ MyPair (r, v)

spec :: Spec
spec = do
  describe "bifoldMapAnnSelectG" $ do
    let singleton :: a -> [a]
        singleton x = [x]

    it "bifoldMapAnnSelectG (const mempty) == foldMap"
      $ hedgehog
      $ do
        annSelectG :: AnnSelectG ('Postgres 'Vanilla) (MyPair ('Postgres 'Vanilla) Int) Int <-
          forAll
            $ genAnnSelectG (int defaultRange) (genMyPair (int defaultRange) (int defaultRange))
        bifoldMapAnnSelectG (const []) singleton annSelectG === foldMap singleton annSelectG
        bifoldMapAnnSelectG singleton (const []) annSelectG === foldMap (foldMap $ bifoldMap singleton (const [])) (_asnFields annSelectG)
