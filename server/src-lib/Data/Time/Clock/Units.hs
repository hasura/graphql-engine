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

NOTE: the 'Real' and 'Fractional' instances just essentially add or strip the unit label (as 
above), so you can't use 'realToFrac' to convert between the units types here. Instead try 
'fromUnits' which is less of a foot-gun.

The 'Read' instances for these types mirror the behavior of the 'RealFrac' instance wrt numeric
literals for convenient serialization (e.g. when working with env vars):

@
>>> read "1.2" :: Milliseconds 
Milliseconds {milliseconds = 0.0012s}
@

Generally, if you need to pass around a duration between functions you should use 'DiffTime'
directly. However if storing a duration in a type that will be serialized, e.g. one having
a 'ToJSON' instance, it is better to use one of these explicit wrapper types so that it's 
obvious what units will be used. -}
module Data.Time.Clock.Units
  ( Days(..)
  , Hours(..)
  , Minutes(..)
  , Seconds(..)
  , Milliseconds(..)
  , Microseconds(..)
  , Nanoseconds(..)
  -- * Converting between units
  , Duration(..)
  , fromUnits
  -- * Reexports
  -- | We use 'DiffTime' as the standard type for unit-agnostic duration in our
  -- code. You'll need to convert to a 'NominalDiffTime'  (with 'fromUnits') in
  -- order to do anything useful with 'UTCTime' with these durations.
  --
  -- NOTE: some care must be taken especially when 'NominalDiffTime' interacts
  -- with 'UTCTime':
  --
  --  - a 'DiffTime' or 'NominalDiffTime' my be negative
  --  - 'addUTCTime' and 'diffUTCTime' do not attempt to handle leap seconds
  , DiffTime
  ) where

import           Prelude

import           Control.Arrow   (first)
import           Data.Aeson
import           Data.Hashable
import           Data.Proxy
import           Data.Time.Clock
import           GHC.TypeLits
import           Numeric         (readFloat)


newtype Seconds = Seconds { seconds :: DiffTime }
  -- NOTE: we want Show to give a pastable data structure string, even
  -- though Read is custom.
  deriving (Duration, Show, Eq, Ord, ToJSON, FromJSON)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit (SecondsP 1))

-- TODO if needed: deriving (ToJSON, FromJSON) via (TimeUnit ..) making sure
--      to copy Aeson instances (with withBoundedScientific), and e.g.
--         toJSON (5 :: Minutes) == Number 5
newtype Days = Days { days :: DiffTime }
  deriving (Duration, Show, Eq, Ord)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit (SecondsP 86400))

newtype Hours = Hours { hours :: DiffTime }
  deriving (Duration, Show, Eq, Ord)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit (SecondsP 3600))

newtype Minutes = Minutes { minutes :: DiffTime }
  deriving (Duration, Show, Eq, Ord)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit (SecondsP 60))

newtype Milliseconds = Milliseconds { milliseconds :: DiffTime }
  deriving (Duration, Show, Eq, Ord)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit 1000000000)

newtype Microseconds = Microseconds { microseconds :: DiffTime }
  deriving (Duration, Show, Eq, Ord)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit 1000000)

newtype Nanoseconds = Nanoseconds { nanoseconds :: DiffTime }
  deriving (Duration, Show, Eq, Ord)
  deriving (Read, Num, Fractional, Real, Hashable, RealFrac) via (TimeUnit 1000)

-- Internal for deriving via
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

instance (KnownNat picosPerUnit) => Read (TimeUnit picosPerUnit) where
  readsPrec _ = map (first fromRational) . readFloat

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

-- we can ignore unit:
instance Hashable (TimeUnit a) where
  hashWithSalt salt (TimeUnit dt) = hashWithSalt salt $ 
    (realToFrac :: DiffTime -> Double) dt


-- | Duration types isomorphic to 'DiffTime', powering 'fromUnits'.
class Duration d where
  fromDiffTime :: DiffTime -> d
  toDiffTime :: d -> DiffTime

instance Duration DiffTime where
  fromDiffTime = id
  toDiffTime = id

instance Duration NominalDiffTime where
  fromDiffTime = realToFrac
  toDiffTime = realToFrac

-- | Safe conversion between duration units.
fromUnits :: (Duration x, Duration y)=> x -> y
fromUnits = fromDiffTime . toDiffTime
