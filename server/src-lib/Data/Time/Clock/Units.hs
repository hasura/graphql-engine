{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE TypeOperators       #-}

{-| Types for time intervals of various units. Each newtype wraps 'DiffTime', but they have
different 'Num' instances. The intent is to use the record selectors to write literals with
particular units, like this:

@
>>> 'milliseconds' 500
0.5s
>>> 'hours' 3
10800s
>>> 'minutes' 1.5 + 'seconds' 30
120s
@

You can also go the other way using the constructors rather than the selectors:

@
>>> 'toRational' '$' 'Minutes' ('seconds' 17)
17 % 60
>>> 'realToFrac' ('Days' ('hours' 12)) :: 'Double'
0.5
@

Generally, it doesn’t make sense to pass these wrappers around or put them inside data structures,
since any function that needs a duration should just accept a 'DiffTime', but they’re useful for
literals and conversions to/from other types. -}
module Data.Time.Clock.Units
  ( Days(..)
  , Hours(..)
  , Minutes(..)
  , Seconds
  , seconds
  , Milliseconds(..)
  , Microseconds(..)
  , Nanoseconds(..)
  ) where

import           Prelude

import           Data.Proxy
import           Data.Time.Clock
import           GHC.TypeLits

type Seconds = DiffTime

seconds :: DiffTime -> DiffTime
seconds = id

newtype Days = Days { days :: DiffTime }
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Real, RealFrac) via (TimeUnit (SecondsP 86400))

newtype Hours = Hours { hours :: DiffTime }
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Real, RealFrac) via (TimeUnit (SecondsP 3600))

newtype Minutes = Minutes { minutes :: DiffTime }
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Real, RealFrac) via (TimeUnit (SecondsP 60))

newtype Milliseconds = Milliseconds { milliseconds :: DiffTime }
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Real, RealFrac) via (TimeUnit 1000000000)

newtype Microseconds = Microseconds { microseconds :: DiffTime }
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Real, RealFrac) via (TimeUnit 1000000)

newtype Nanoseconds = Nanoseconds { nanoseconds :: DiffTime }
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Real, RealFrac) via (TimeUnit 1000)

newtype TimeUnit (picosPerUnit :: Nat) = TimeUnit DiffTime
  deriving (Show, Eq, Ord)

type SecondsP n = n GHC.TypeLits.* 1000000000000

natNum :: forall n a. (KnownNat n, Num a) => a
natNum = fromInteger $ natVal (Proxy @n)

instance (KnownNat picosPerUnit) => Num (TimeUnit picosPerUnit) where
  TimeUnit a + TimeUnit b = TimeUnit $ a + b
  TimeUnit a - TimeUnit b = TimeUnit $ a - b
  TimeUnit a * TimeUnit b = TimeUnit . picosecondsToDiffTime $
    diffTimeToPicoseconds a * diffTimeToPicoseconds b `div` natNum @picosPerUnit
  negate (TimeUnit a) = TimeUnit $ negate a
  abs (TimeUnit a) = TimeUnit $ abs a
  signum (TimeUnit a) = TimeUnit $ signum a
  fromInteger a = TimeUnit . picosecondsToDiffTime $ a * natNum @picosPerUnit

instance (KnownNat picosPerUnit) => Fractional (TimeUnit picosPerUnit) where
  TimeUnit a / TimeUnit b = TimeUnit . picosecondsToDiffTime $
    diffTimeToPicoseconds a * natNum @picosPerUnit `div` diffTimeToPicoseconds b
  fromRational a = TimeUnit . picosecondsToDiffTime $ round (a * natNum @picosPerUnit)

instance (KnownNat picosPerUnit) => Real (TimeUnit picosPerUnit) where
  toRational (TimeUnit a) = toRational (diffTimeToPicoseconds a) / natNum @picosPerUnit

instance (KnownNat picosPerUnit) => RealFrac (TimeUnit picosPerUnit) where
  properFraction a = (i, a - fromIntegral i)
    where i = truncate a
  truncate = truncate . toRational
  round = round . toRational
  ceiling = ceiling . toRational
  floor = floor . toRational
