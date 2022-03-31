module Test.Autodocodec.Extended (genValueWrapper, genValueWrapper2, genValueWrapper3) where

import Autodocodec.Extended
import Hasura.Prelude
import Hedgehog (MonadGen)

genValueWrapper :: MonadGen m => m a -> m (ValueWrapper t a)
genValueWrapper = fmap ValueWrapper

genValueWrapper2 :: MonadGen m => m a1 -> m a2 -> m (ValueWrapper2 t1 a1 t2 a2)
genValueWrapper2 genA1 genA2 =
  ValueWrapper2
    <$> genA1
    <*> genA2

genValueWrapper3 :: MonadGen m => m a1 -> m a2 -> m a3 -> m (ValueWrapper3 t1 a1 t2 a2 t3 a3)
genValueWrapper3 genA1 genA2 genA3 =
  ValueWrapper3
    <$> genA1
    <*> genA2
    <*> genA3
