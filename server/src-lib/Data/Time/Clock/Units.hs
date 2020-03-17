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
import           Data.Aeson.Internal.Time (fromPico, toPico)
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

deriving instance AsDuration (Seconds 'Absolute)
deriving instance AsDuration (Seconds 'Calendar)

deriving instance ToJSON (Seconds 'Absolute)
deriving instance ToJSON (Seconds 'Calendar)

deriving instance FromJSON (Seconds 'Absolute)
deriving instance FromJSON (Seconds 'Calendar)

-- NOTE: we want Show to give a pastable data structure string, even
-- though Read is custom.
deriving instance Show (Seconds 'Absolute)
deriving instance Show (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => Eq (Seconds t)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => Ord (Seconds t)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => Read (Seconds t)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => Num (Seconds t)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => Fractional (Seconds t)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => Real (Seconds t)

deriving via (TimeUnit (SecondsP 1) 'Absolute) instance Hashable (Seconds 'Absolute)
deriving via (TimeUnit (SecondsP 1) 'Calendar) instance Hashable (Seconds 'Calendar)

deriving via (TimeUnit (SecondsP 1) t) instance (AsPicoseconds t) => RealFrac (Seconds t)

-- TODO if needed: deriving (ToJSON, FromJSON) via (TimeUnit ..) making sure
--      to copy Aeson instances (with withBoundedScientific), and e.g.
--         toJSON (5 :: Minutes) == Number 5
newtype Days t = Days { days :: Duration t}

deriving instance AsDuration (Days 'Absolute)
deriving instance AsDuration (Days 'Calendar)

deriving instance Show (Days 'Absolute)
deriving instance Show (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => Eq (Days t)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => Ord (Days t)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => Read (Days t)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => Num (Days t)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => Fractional (Days t)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => Real (Days t)

deriving via (TimeUnit (SecondsP 86400) 'Absolute) instance Hashable (Days 'Absolute)
deriving via (TimeUnit (SecondsP 86400) 'Calendar) instance Hashable (Days 'Calendar)

deriving via (TimeUnit (SecondsP 86400) t) instance (AsPicoseconds t) => RealFrac (Days t)


newtype Hours t = Hours { hours :: Duration t}

deriving instance AsDuration (Hours 'Absolute)
deriving instance AsDuration (Hours 'Calendar)

deriving instance Show (Hours 'Absolute)
deriving instance Show (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => Eq (Hours t)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => Ord (Hours t)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => Read (Hours t)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => Num (Hours t)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => Fractional (Hours t)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => Real (Hours t)

deriving via (TimeUnit (SecondsP 3600) 'Absolute) instance Hashable (Hours 'Absolute)
deriving via (TimeUnit (SecondsP 3600) 'Calendar) instance Hashable (Hours 'Calendar)

deriving via (TimeUnit (SecondsP 3600) t) instance (AsPicoseconds t) => RealFrac (Hours t)

newtype Minutes t = Minutes { minutes :: Duration t}

deriving instance AsDuration (Minutes 'Absolute)
deriving instance AsDuration (Minutes 'Calendar)

deriving instance Show (Minutes 'Absolute)
deriving instance Show (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => Eq (Minutes t)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => Ord (Minutes t)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => Read (Minutes t)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => Num (Minutes t)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => Fractional (Minutes t)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => Real (Minutes t)

deriving via (TimeUnit (SecondsP 60) 'Absolute) instance Hashable (Minutes 'Absolute)
deriving via (TimeUnit (SecondsP 60) 'Calendar) instance Hashable (Minutes 'Calendar)

deriving via (TimeUnit (SecondsP 60) t) instance (AsPicoseconds t) => RealFrac (Minutes t)

newtype Milliseconds t = Milliseconds { milliseconds :: Duration t}

deriving instance AsDuration (Milliseconds 'Absolute)
deriving instance AsDuration (Milliseconds 'Calendar)

deriving instance Show (Milliseconds 'Absolute)
deriving instance Show (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => Eq (Milliseconds t)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => Ord (Milliseconds t)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => Read (Milliseconds t)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => Num (Milliseconds t)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => Fractional (Milliseconds t)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => Real (Milliseconds t)

deriving via (TimeUnit 1000000000 'Absolute) instance Hashable (Milliseconds 'Absolute)
deriving via (TimeUnit 1000000000 'Calendar) instance Hashable (Milliseconds 'Calendar)

deriving via (TimeUnit 1000000000 t) instance (AsPicoseconds t) => RealFrac (Milliseconds t)


newtype Microseconds t = Microseconds { microseconds :: Duration t}

deriving instance AsDuration (Microseconds 'Absolute)
deriving instance AsDuration (Microseconds 'Calendar)

deriving instance Show (Microseconds 'Absolute)
deriving instance Show (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => Eq (Microseconds t)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => Ord (Microseconds t)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => Read (Microseconds t)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => Num (Microseconds t)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => Fractional (Microseconds t)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => Real (Microseconds t)

deriving via (TimeUnit 1000000 'Absolute) instance Hashable (Microseconds 'Absolute)
deriving via (TimeUnit 1000000 'Calendar) instance Hashable (Microseconds 'Calendar)

deriving via (TimeUnit 1000000 t) instance (AsPicoseconds t) => RealFrac (Microseconds t)

newtype Nanoseconds t = Nanoseconds { nanoseconds :: Duration t}

deriving instance AsDuration (Nanoseconds 'Absolute)
deriving instance AsDuration (Nanoseconds 'Calendar)

deriving instance Show (Nanoseconds 'Absolute)
deriving instance Show (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => Eq (Nanoseconds t)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => Ord (Nanoseconds t)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => Read (Nanoseconds t)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => Num (Nanoseconds t)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => Fractional (Nanoseconds t)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => Real (Nanoseconds t)

deriving via (TimeUnit 1000 'Absolute) instance Hashable (Nanoseconds 'Absolute)
deriving via (TimeUnit 1000 'Calendar) instance Hashable (Nanoseconds 'Calendar)

deriving via (TimeUnit 1000 t) instance (AsPicoseconds t) => RealFrac (Nanoseconds t)

-- Internal for deriving via
newtype TimeUnit (picosPerUnit :: Nat) t = TimeUnit (Duration t)

deriving instance Show (TimeUnit picosPerUnit 'Absolute)
deriving instance Show (TimeUnit picosPerUnit 'Calendar)

deriving instance Eq (TimeUnit picosPerUnit 'Absolute)
deriving instance Eq (TimeUnit picosPerUnit 'Calendar)

deriving instance Integral Pico

type SecondsP n = n GHC.TypeLits.* 1000000000000

natNum :: forall n a. (KnownNat n, Num a) => a
natNum = fromInteger $ natVal (Proxy @n)

class AsPicoseconds t where
    toPicoseconds :: Duration t -> Pico
    fromPicoseconds :: Pico -> Duration t

instance AsPicoseconds 'Absolute where
    toPicoseconds = toPico . diffTimeToPicoseconds
    fromPicoseconds = picosecondsToDiffTime . fromPico

instance AsPicoseconds 'Calendar where
    toPicoseconds = nominalDiffTimeToSeconds
    fromPicoseconds = secondsToNominalDiffTime

instance (KnownNat picosPerUnit, AsPicoseconds t) => Eq (TimeUnit picosPerUnit t) where
    TimeUnit a == TimeUnit b = toPicoseconds a == toPicoseconds b

instance (KnownNat picosPerUnit, AsPicoseconds t) => Ord (TimeUnit picosPerUnit t) where
    compare (TimeUnit a) (TimeUnit b) = compare (toPicoseconds a) (toPicoseconds b)

instance (KnownNat picosPerUnit, AsPicoseconds t) => Num (TimeUnit picosPerUnit t) where
  TimeUnit a + TimeUnit b = TimeUnit $ fromPicoseconds $ (toPicoseconds a) + (toPicoseconds b)
  TimeUnit a - TimeUnit b = TimeUnit $ fromPicoseconds $ (toPicoseconds a) - (toPicoseconds b)
  TimeUnit a * TimeUnit b = TimeUnit . fromPicoseconds $
    toPicoseconds a * toPicoseconds b `div` natNum @picosPerUnit
  negate (TimeUnit a) = TimeUnit $ fromPicoseconds $ negate $ (toPicoseconds a)
  abs (TimeUnit a) = TimeUnit $ fromPicoseconds $ abs $ (toPicoseconds a)
  signum (TimeUnit a) = TimeUnit $ fromPicoseconds $ signum $ (toPicoseconds a)
  fromInteger a = TimeUnit . fromPicoseconds $ toPico $ a * natNum @picosPerUnit

instance (KnownNat picosPerUnit, AsPicoseconds t) => Read (TimeUnit picosPerUnit t) where
  readsPrec _ = map (first fromRational) . readFloat

instance (KnownNat picosPerUnit, AsPicoseconds t) => Fractional (TimeUnit picosPerUnit t) where
  TimeUnit a / TimeUnit b = TimeUnit . fromPicoseconds $
    toPicoseconds a * natNum @picosPerUnit `div` toPicoseconds b
  fromRational a = TimeUnit . fromPicoseconds $ round (a * natNum @picosPerUnit)

instance (KnownNat picosPerUnit, AsPicoseconds t) => Real (TimeUnit picosPerUnit t) where
  toRational (TimeUnit a) = toRational (toPicoseconds a) / natNum @picosPerUnit

instance (KnownNat picosPerUnit, AsPicoseconds t) => RealFrac (TimeUnit picosPerUnit t) where
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

-- we can ignore unit:
instance Hashable (TimeUnit a 'Calendar) where
  hashWithSalt salt (TimeUnit dt) = hashWithSalt salt $
    (realToFrac :: NominalDiffTime -> Double) dt

-- | Duration types isomorphic to 'DiffTime', powering 'fromUnits'.
class AsDuration d where
  fromDiffTime :: DiffTime -> d
  toDiffTime :: d -> DiffTime

instance AsDuration DiffTime where
  fromDiffTime = id
  toDiffTime = id

instance AsDuration NominalDiffTime where
  fromDiffTime = realToFrac
  toDiffTime = realToFrac

-- | Safe conversion between duration units.
fromUnits :: (AsDuration x, AsDuration y)=> x -> y
fromUnits = fromDiffTime . toDiffTime

-- | The input to this function is the number of picos in Integer
-- and then converting it into seconds.
toFixedPico :: Integer -> Pico
toFixedPico = (/1000000000000) . fromIntegral
