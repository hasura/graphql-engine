{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RoleAnnotations #-}

module Hasura.Incremental.Select
  ( Select(..)
  , ConstS(..)
  , selectKey
  , UniqueS
  , newUniqueS
  , DMapS(..)

  -- * Re-exports
  , GEq(..)
  , GCompare(..)
  , GOrdering(..)
  , (:~:)(..)
  ) where

import           Hasura.Prelude

import qualified Data.Dependent.Map   as DM
import qualified Data.HashMap.Strict  as M

import           Control.Monad.Unique
import           Data.GADT.Compare
import           Data.Kind
import           Unsafe.Coerce        (unsafeCoerce)

-- | The 'Select' class provides a way to access subparts of a product type using a reified
-- 'Selector'. A @'Selector' a b@ is essentially a function from @a@ to @b@, and indeed 'select'
-- converts a 'Selector' to such a function. However, unlike functions, 'Selector's can be compared
-- for equality using 'GEq' and ordered using 'GCompare'.
--
-- This is useful to implement dependency tracking, since it’s possible to track in a reified form
-- exactly which parts of a data structure are used.
class (GCompare (Selector a)) => Select a where
  type Selector a :: Type -> Type
  select :: Selector a b -> a -> b

instance (Eq k, Ord k, Hashable k) => Select (HashMap k v) where
  type Selector (HashMap k v) = ConstS k (Maybe v)
  select (ConstS k) = M.lookup k

instance (GCompare k) => Select (DM.DMap k f) where
  type Selector (DM.DMap k f) = DMapS k f
  select (DMapS k) = DM.lookup k

-- | The constant selector, which is useful for representing selectors into data structures where
-- all fields have the same type. Matching on a value of type @'ConstS' k a b@ causes @a@ and @b@ to
-- unify, effectively “pinning” @b@ to @a@.
data ConstS k a b where
  ConstS :: !k -> ConstS k a a

selectKey :: (Select a, Selector a ~ ConstS k v) => k -> a -> v
selectKey = select . ConstS

instance (Eq k) => GEq (ConstS k a) where
  ConstS a `geq` ConstS b
    | a == b    = Just Refl
    | otherwise = Nothing

instance (Ord k) => GCompare (ConstS k a) where
  ConstS a `gcompare` ConstS b = case compare a b of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT

-- | A 'UniqueS' is, as the name implies, a globally-unique 'Selector', which can be created using
-- 'newUniqueS'. If a value of type @'UniqueS' a@ is found to be equal (via 'geq') with another
-- value of type @'UniqueS' b@, then @a@ and @b@ must be the same type. This effectively allows the
-- creation of a dynamically-extensible sum type, where new constructors can be created at runtime
-- using 'newUniqueS'.
type role UniqueS nominal
newtype UniqueS a = UniqueS Unique
  deriving (Eq)

newUniqueS :: (MonadUnique m) => m (UniqueS a)
newUniqueS = UniqueS <$> newUnique
{-# INLINE newUniqueS #-}

instance GEq UniqueS where
  UniqueS a `geq` UniqueS b
    -- This use of `unsafeCoerce` is safe as long as we don’t export the constructor of `UniqueS`.
    -- Because a `UniqueS` is, in fact, unique, then we can be certain that equality of 'UniqueS's
    -- implies equality of their argument types.
    | a == b    = Just (unsafeCoerce Refl)
    | otherwise = Nothing

instance GCompare UniqueS where
  UniqueS a `gcompare` UniqueS b = case compare a b of
    LT -> GLT
    -- See note about `unsafeCoerce` above.
    EQ -> unsafeCoerce GEQ
    GT -> GGT

data DMapS k f a where
  DMapS :: !(k a) -> DMapS k f (Maybe (f a))

instance (GEq k) => GEq (DMapS k f) where
  DMapS a `geq` DMapS b = case a `geq` b of
    Just Refl -> Just Refl
    Nothing   -> Nothing

instance (GCompare k) => GCompare (DMapS k f) where
  DMapS a `gcompare` DMapS b = case a `gcompare` b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
