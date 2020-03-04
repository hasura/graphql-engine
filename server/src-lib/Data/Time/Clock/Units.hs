{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

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
  , Duration
  , Duration'(..)
  , DurationType(..)
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
import           Data.Fixed
import           Data.Hashable
import           Data.Proxy
import           Data.Time.Clock
import           GHC.TypeLits
import           Numeric         (readFloat)


data DurationType = Absolute | Calendar

type family Duration a = r | r -> a where
  Duration 'Absolute = DiffTime
  Duration 'Calendar = NominalDiffTime

newtype Seconds t = Seconds { seconds :: Duration t}

deriving instance Duration' (Seconds 'Absolute)
deriving instance Duration' (Seconds 'Calendar)

-- NOTE: we want Show to give a pastable data structure string, even
-- though Read is custom.
deriving instance Show (Seconds 'Absolute)
deriving instance Show (Seconds 'Calendar)

deriving instance Eq (Seconds 'Absolute)
deriving instance Eq (Seconds 'Calendar)

deriving instance Ord (Seconds 'Absolute)
deriving instance Ord (Seconds 'Calendar)

deriving instance ToJSON (Seconds 'Absolute)
deriving instance ToJSON (Seconds 'Calendar)

deriving instance FromJSON (Seconds 'Absolute)
deriving instance FromJSON (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance Read (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance Read (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance Num (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance Num (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance Fractional (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance Fractional (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance Real (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance Real (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance Hashable (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance Hashable (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance RealFrac (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance RealFrac (Seconds 'Calendar)


-- TODO if needed: deriving (ToJSON, FromJSON) via (TimeUnit ..) making sure
--      to copy Aeson instances (with withBoundedScientific), and e.g.
--         toJSON (5 :: Minutes) == Number 5
newtype Days t = Days { days :: Duration t}

deriving instance Duration' (Days 'Absolute)
deriving instance Duration' (Days 'Calendar)

deriving instance Show (Days 'Absolute)
deriving instance Show (Days 'Calendar)

deriving instance Eq (Days 'Absolute)
deriving instance Eq (Days 'Calendar)

deriving instance Ord (Days 'Absolute)
deriving instance Ord (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance Read (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance Read (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance Num (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance Num (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance Fractional (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance Fractional (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance Real (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance Real (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance Hashable (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance Hashable (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance RealFrac (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance RealFrac (Days 'Calendar)

newtype Hours t = Hours { hours :: Duration t}

deriving instance Duration' (Hours 'Absolute)
deriving instance Duration' (Hours 'Calendar)

deriving instance Show (Hours 'Absolute)
deriving instance Show (Hours 'Calendar)

deriving instance Eq (Hours 'Absolute)
deriving instance Eq (Hours 'Calendar)

deriving instance Ord (Hours 'Absolute)
deriving instance Ord (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance Read (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance Read (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance Num (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance Num (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance Fractional (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance Fractional (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance Real (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance Real (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance Hashable (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance Hashable (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance RealFrac (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance RealFrac (Hours 'Calendar)

newtype Minutes t = Minutes { minutes :: Duration t}

deriving instance Duration' (Minutes 'Absolute)
deriving instance Duration' (Minutes 'Calendar)

deriving instance Show (Minutes 'Absolute)
deriving instance Show (Minutes 'Calendar)

deriving instance Eq (Minutes 'Absolute)
deriving instance Eq (Minutes 'Calendar)

deriving instance Ord (Minutes 'Absolute)
deriving instance Ord (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance Read (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance Read (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance Num (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance Num (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance Fractional (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance Fractional (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance Real (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance Real (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance Hashable (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance Hashable (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance RealFrac (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance RealFrac (Minutes 'Calendar)

newtype Milliseconds t = Milliseconds { milliseconds :: Duration t}

deriving instance Duration' (Milliseconds 'Absolute)
deriving instance Duration' (Milliseconds 'Calendar)

deriving instance Show (Milliseconds 'Absolute)
deriving instance Show (Milliseconds 'Calendar)

deriving instance Eq (Milliseconds 'Absolute)
deriving instance Eq (Milliseconds 'Calendar)

deriving instance Ord (Milliseconds 'Absolute)
deriving instance Ord (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 'Absolute) instance Read (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance Read (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 'Absolute) instance Num (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance Num (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 'Absolute) instance Fractional (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance Fractional (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 'Absolute) instance Real (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance Real (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 'Absolute) instance Hashable (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance Hashable (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 'Absolute) instance RealFrac (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance RealFrac (Milliseconds 'Calendar)


newtype Microseconds t = Microseconds { microseconds :: Duration t}

deriving instance Duration' (Microseconds 'Absolute)
deriving instance Duration' (Microseconds 'Calendar)

deriving instance Show (Microseconds 'Absolute)
deriving instance Show (Microseconds 'Calendar)

deriving instance Eq (Microseconds 'Absolute)
deriving instance Eq (Microseconds 'Calendar)

deriving instance Ord (Microseconds 'Absolute)
deriving instance Ord (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 'Absolute) instance Read (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance Read (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 'Absolute) instance Num (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance Num (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 'Absolute) instance Fractional (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance Fractional (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 'Absolute) instance Real (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance Real (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 'Absolute) instance Hashable (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance Hashable (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 'Absolute) instance RealFrac (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance RealFrac (Microseconds 'Calendar)


newtype Nanoseconds t = Nanoseconds { nanoseconds :: Duration t}

deriving instance Duration' (Nanoseconds 'Absolute)
deriving instance Duration' (Nanoseconds 'Calendar)

deriving instance Show (Nanoseconds 'Absolute)
deriving instance Show (Nanoseconds 'Calendar)

deriving instance Eq (Nanoseconds 'Absolute)
deriving instance Eq (Nanoseconds 'Calendar)

deriving instance Ord (Nanoseconds 'Absolute)
deriving instance Ord (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 'Absolute) instance Read (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance Read (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 'Absolute) instance Num (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance Num (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 'Absolute) instance Fractional (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance Fractional (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 'Absolute) instance Real (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance Real (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 'Absolute) instance Hashable (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance Hashable (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 'Absolute) instance RealFrac (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance RealFrac (Nanoseconds 'Calendar)

-- Internal for deriving via
newtype TimeUnit (picosPerUnit :: Nat) t = TimeUnit (Duration t)

deriving instance Show (TimeUnit picosPerUnit 'Absolute)
deriving instance Show (TimeUnit picosPerUnit 'Calendar)

deriving instance Eq (TimeUnit picosPerUnit 'Absolute)
deriving instance Eq (TimeUnit picosPerUnit 'Calendar)

deriving instance Ord (TimeUnit picosPerUnit 'Absolute)
deriving instance Ord (TimeUnit picosPerUnit 'Calendar)

type SecondsP n = n GHC.TypeLits.* 1000000000000

natNum :: forall n a. (KnownNat n, Num a) => a
natNum = fromInteger $ natVal (Proxy @n)

instance (KnownNat picosPerUnit) => Num (TimeUnit picosPerUnit 'Absolute) where
  TimeUnit a + TimeUnit b = TimeUnit $ a + b
  TimeUnit a - TimeUnit b = TimeUnit $ a - b
  TimeUnit a * TimeUnit b = TimeUnit . picosecondsToDiffTime $
    diffTimeToPicoseconds a * diffTimeToPicoseconds b `div` natNum @picosPerUnit
  negate (TimeUnit a) = TimeUnit $ negate a
  abs (TimeUnit a) = TimeUnit $ abs a
  signum (TimeUnit a) = TimeUnit $ signum a
  fromInteger a = TimeUnit . picosecondsToDiffTime $ a * natNum @picosPerUnit

instance (KnownNat picosPerUnit) => Read (TimeUnit picosPerUnit 'Absolute) where
  readsPrec _ = map (first fromRational) . readFloat

instance (KnownNat picosPerUnit) => Fractional (TimeUnit picosPerUnit 'Absolute) where
  TimeUnit a / TimeUnit b = TimeUnit . picosecondsToDiffTime $
    diffTimeToPicoseconds a * natNum @picosPerUnit `div` diffTimeToPicoseconds b
  fromRational a = TimeUnit . picosecondsToDiffTime $ round (a * natNum @picosPerUnit)

instance (KnownNat picosPerUnit) => Real (TimeUnit picosPerUnit 'Absolute) where
  toRational (TimeUnit a) = toRational (diffTimeToPicoseconds a) / natNum @picosPerUnit

instance (KnownNat picosPerUnit) => RealFrac (TimeUnit picosPerUnit 'Absolute) where
  properFraction a = (i, a - fromIntegral i)
    where i = truncate a
  truncate = truncate . toRational
  round = round . toRational
  ceiling = ceiling . toRational
  floor = floor . toRational

-- we can ignore unit:
instance Hashable (TimeUnit a 'Absolute) where
  hashWithSalt salt (TimeUnit dt) = hashWithSalt salt $
    (realToFrac :: DiffTime -> Double) dt


-- instances for Calendar
instance (KnownNat picosPerUnit) => Num (TimeUnit picosPerUnit 'Calendar) where
  TimeUnit a + TimeUnit b = TimeUnit $ a + b
  TimeUnit a - TimeUnit b = TimeUnit $ a - b
  TimeUnit a * TimeUnit b = TimeUnit . secondsToNominalDiffTime . toFixedPico $
    (toRational $ nominalDiffTimeToSeconds a * nominalDiffTimeToSeconds b) `div'` (natNum @picosPerUnit)
  negate (TimeUnit a) = TimeUnit $ negate a
  abs (TimeUnit a) = TimeUnit $ abs a
  signum (TimeUnit a) = TimeUnit $ signum a
  fromInteger a = TimeUnit . secondsToNominalDiffTime . toFixedPico $ a * natNum @picosPerUnit

instance (KnownNat picosPerUnit) => Read (TimeUnit picosPerUnit 'Calendar) where
  readsPrec _ = map (first fromRational) . readFloat

instance (KnownNat picosPerUnit) => Fractional (TimeUnit picosPerUnit 'Calendar) where
  TimeUnit a / TimeUnit b = TimeUnit . secondsToNominalDiffTime . toFixedPico $ fromInteger $
    ((toRational $ nominalDiffTimeToSeconds a) * natNum @picosPerUnit) `div'` (toRational $ nominalDiffTimeToSeconds b)
  fromRational a = TimeUnit . secondsToNominalDiffTime . toFixedPico $ round (a * natNum @picosPerUnit)

instance (KnownNat picosPerUnit) => Real (TimeUnit picosPerUnit 'Calendar) where
  toRational (TimeUnit a) = toRational (nominalDiffTimeToSeconds a) / natNum @picosPerUnit

instance (KnownNat picosPerUnit) => RealFrac (TimeUnit picosPerUnit 'Calendar) where
  properFraction a = (i, a - fromIntegral i)
    where i = truncate a
  truncate = truncate . toRational
  round = round . toRational
  ceiling = ceiling . toRational
  floor = floor . toRational

-- we can ignore unit:
instance Hashable (TimeUnit a 'Calendar) where
  hashWithSalt salt (TimeUnit dt) = hashWithSalt salt $
    (realToFrac :: NominalDiffTime -> Double) dt

-- | Duration types isomorphic to 'DiffTime', powering 'fromUnits'.
class Duration' d where
  fromDiffTime :: DiffTime -> d
  toDiffTime :: d -> DiffTime

instance Duration' DiffTime where
  fromDiffTime = id
  toDiffTime = id

instance Duration' NominalDiffTime where
  fromDiffTime = realToFrac
  toDiffTime = realToFrac

-- | Safe conversion between duration units.
fromUnits :: (Duration' x, Duration' y)=> x -> y
fromUnits = fromDiffTime . toDiffTime

toFixedPico :: Integer -> Pico
toFixedPico = fromIntegral
