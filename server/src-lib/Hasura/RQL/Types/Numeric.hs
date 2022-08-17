{-# LANGUAGE ViewPatterns #-}

-- | Utility types relating to numeric values
module Hasura.RQL.Types.Numeric
  ( NonNegative,
    getNonNegative,
    mkNonNegative,
    unsafeNonNegative,
    NonNegativeInt,
    getNonNegativeInt,
    mkNonNegativeInt,
    unsafeNonNegativeInt,
    PositiveInt,
    getPositiveInt,
    mkPositiveInt,
    unsafePositiveInt,
    NonNegativeDiffTime,
    unNonNegativeDiffTime,
    unsafeNonNegativeDiffTime,
    mkNonNegativeDiffTime,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Scientific qualified as Scientific
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

--------------------------------------------------------------------------------

newtype NonNegative a = NonNegative {getNonNegative :: a}
  deriving stock (Functor)
  deriving newtype (Show, Eq, Ord, ToJSON, Generic, NFData, Cacheable, Hashable)

mkNonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
mkNonNegative x = case x >= 0 of
  True -> Just $ NonNegative x
  False -> Nothing

unsafeNonNegative :: a -> NonNegative a
unsafeNonNegative = NonNegative

instance (Fractional a, FromJSON a) => FromJSON (NonNegative a) where
  parseJSON = Aeson.withScientific "NonNegative" $ \t -> do
    case t >= 0 of
      True -> pure $ NonNegative . realToFrac $ t
      False -> fail "negative value not allowed"

newtype NonNegativeInt = NonNegativeInt {getNonNegativeInt :: Int}
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Hashable)

mkNonNegativeInt :: Int -> Maybe NonNegativeInt
mkNonNegativeInt x = case x >= 0 of
  True -> Just $ NonNegativeInt x
  False -> Nothing

unsafeNonNegativeInt :: Int -> NonNegativeInt
unsafeNonNegativeInt = NonNegativeInt

instance FromJSON NonNegativeInt where
  parseJSON = Aeson.withScientific "NonNegativeInt" $ \t -> do
    case t >= 0 of
      True -> maybe (fail "integer passed is out of bounds") (pure . NonNegativeInt) $ Scientific.toBoundedInteger t
      False -> fail "negative value not allowed"

newtype PositiveInt = PositiveInt {getPositiveInt :: Int}
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Hashable)

mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt x = case x > 0 of
  True -> Just $ PositiveInt x
  False -> Nothing

unsafePositiveInt :: Int -> PositiveInt
unsafePositiveInt = PositiveInt

instance FromJSON PositiveInt where
  parseJSON = Aeson.withScientific "NonNegativeInt" $ \t -> do
    case t > 0 of
      True -> maybe (fail "integer passed is out of bounds") (pure . PositiveInt) $ Scientific.toBoundedInteger t
      False -> fail "integer passed is out of bounds"

newtype NonNegativeDiffTime = NonNegativeDiffTime {unNonNegativeDiffTime :: DiffTime}
  deriving (Show, Eq, ToJSON, Generic, NFData, Cacheable, Num)

unsafeNonNegativeDiffTime :: DiffTime -> NonNegativeDiffTime
unsafeNonNegativeDiffTime = NonNegativeDiffTime

mkNonNegativeDiffTime :: DiffTime -> Maybe NonNegativeDiffTime
mkNonNegativeDiffTime x = case x >= 0 of
  True -> Just $ NonNegativeDiffTime x
  False -> Nothing

instance FromJSON NonNegativeDiffTime where
  parseJSON = Aeson.withScientific "NonNegativeDiffTime" $ \t -> do
    case t >= 0 of
      True -> return $ NonNegativeDiffTime . realToFrac $ t
      False -> fail "negative value not allowed"
