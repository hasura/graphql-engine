{-# LANGUAGE UndecidableInstances #-}

{- |

Generic re-implementation of Arbitrary, that discards unrepresentable
values. Modified from the existing generic-arbitrary.

This is a parallel reimplementation of Arbitrary, in which each type might fail
to produce a value, if it contains a dreaded @Void@ field or any otherwise
unrepresentable type that cannot have a 'Arbitrary' instance. The default
generic implementation automatically disregard constructors whose walues
contains such a field, allowing all strctures with other constructors to suceed.

-}

module Test.QuickCheck.Arbitrary.Partial
  ( PartialArbitrary (..)
  , genericPartialArbitrary
  ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Proxy
import           Data.Void
import           GHC.Generics
import           GHC.TypeLits
import           Prelude
import           Test.QuickCheck


-- | A partial version of Arbitrary, for types that cannot or might not be able
-- to have an instance because of unrepresentable types such as Void. By
-- default, the generic implementation implements it by recursively calling
-- `partialArbitrary`. Any branch that fails to produce a value gets discarded.
--
-- At the top of the tree: a type that needs arbitrary but has potentially
-- unrepresentable fields can implement its own arbitrary instance with:
--
--     instance Arbitrary MyType where
--       arbitrary = fromJust partialArbitrary
--
-- This will succeed as long as there's one possible representation.
--
-- At the bottom of the tree: a global overlappable instance means you only need
-- to provide PartialArbitrary instances for said unrepresentable types, or
-- containers; any other type will use this catch-all PartialArbitrary instance,
-- which delegates back to Arbitrary.

class PartialArbitrary a where
  partialArbitrary :: Maybe (Gen a)
  default partialArbitrary :: (Generic a, GPArbitrary ga, ga ~ Rep a) => Maybe (Gen a)
  partialArbitrary = genericPartialArbitrary

instance PartialArbitrary Void where
  partialArbitrary = Nothing

instance (PartialArbitrary a, PartialArbitrary b) => PartialArbitrary (a,b) where
  partialArbitrary = liftA2 (,) <$> partialArbitrary <*> partialArbitrary

instance PartialArbitrary a => PartialArbitrary [a] where
  partialArbitrary = liftArbitrary <$> partialArbitrary

instance PartialArbitrary a => PartialArbitrary (Maybe a) where
  partialArbitrary = liftArbitrary <$> partialArbitrary

instance {-# OVERLAPPABLE #-} Arbitrary a => PartialArbitrary a where
  partialArbitrary = Just arbitrary

genericPartialArbitrary :: (Generic a, GPArbitrary ga, ga ~ Rep a) => Maybe (Gen a)
genericPartialArbitrary = (fmap . fmap) to gPArbitrary


-- | Generic version of PartialArbitrary, used to implement
-- 'genericPartialArbitrary'. This was originally taken verbatim from
-- generic-arbitrary but was adapted for our @Maybe@ approach.

class GPArbitrary a where
  gPArbitrary :: Maybe (Gen (a x))

instance GPArbitrary U1 where
  gPArbitrary = Just $ pure U1

instance PartialArbitrary c => GPArbitrary (K1 i c) where
  gPArbitrary = (fmap . fmap) K1 partialArbitrary

instance GPArbitrary f => GPArbitrary (M1 i c f) where
  gPArbitrary = (fmap . fmap) M1 gPArbitrary

instance (GPArbitrary a, GPArbitrary b) => GPArbitrary (a :*: b) where
  gPArbitrary = (liftA2 . liftA2) (:*:) gPArbitrary gPArbitrary

instance
  ( GPArbitrary a
  , GPArbitrary b
  , KnownNat (SumLen a)
  , KnownNat (SumLen b)
  ) => GPArbitrary (a :+: b) where
  gPArbitrary = case (l1, r1) of
    -- both branches are representable: distribute evenly
    (Just a, Just b) -> Just $ frequency [(lfreq, a), (rfreq, b)]
    -- pick whichever is representable
    (a,      b     ) -> a <|> b
    where
      l1 = (fmap . fmap) L1 gPArbitrary
      r1 = (fmap . fmap) R1 gPArbitrary
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))


-- | Calculates count of constructors encoded by particular ':+:'. This is used
-- to ensure that we consider all constructors of a type evenly.

type family SumLen a :: Nat where
  SumLen (a :+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1
