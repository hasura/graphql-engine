{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Hasura.Incremental.Select
  ( Select (..),
    ConstS (..),
    selectKey,
    FMapS (..),
    FieldS (..),
    UniqueS,
    newUniqueS,
    DMapS (..),

    -- * Re-exports
    GEq (..),
    GCompare (..),
    GOrdering (..),
    (:~:) (..),
  )
where

import Data.Dependent.Map qualified as DM
import Data.HashMap.Strict qualified as HashMap
import Data.Kind
import Data.Proxy (Proxy (..))
import Data.Type.Equality
import Data.Unique (Unique, newUnique)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, sameSymbol, symbolVal)
import Hasura.Prelude
import Unsafe.Coerce (unsafeCoerce)
import "dependent-sum" Data.GADT.Compare

-- | The 'Select' class provides a way to access subparts of a product type using a reified
-- 'Selector'. A @'Selector' a b@ is essentially a function from @a@ to @b@, and indeed 'select'
-- converts a 'Selector' to such a function. However, unlike functions, 'Selector's can be compared
-- for equality using 'GEq' and ordered using 'GCompare'.
--
-- This is useful to implement dependency tracking, since it’s possible to track in a reified form
-- exactly which parts of a data structure are used.
--
-- Instances of 'Select' can be automatically derived for record types (just define an empty
-- instance). The instance uses the magical 'HasField' constraints, and 'Selector's for the type can
-- be written using @OverloadedLabels@.
class (GCompare (Selector a)) => Select a where
  type Selector a :: Type -> Type
  select :: Selector a b -> a -> b

  type Selector r = FieldS r
  default select :: (Selector a ~ FieldS a) => Selector a b -> a -> b
  select (FieldS (_ :: Proxy s)) = getField @s

instance (Ord k, Hashable k) => Select (HashMap k v) where
  type Selector (HashMap k v) = ConstS k (Maybe v)
  select (ConstS k) = HashMap.lookup k

instance (GCompare k) => Select (DM.DMap k f) where
  type Selector (DM.DMap k f) = DMapS k f
  select (DMapS k) = DM.lookup k

newtype FMap f x = FMap {unFMap :: f x}
  deriving (Functor)

data FMapS f a b where
  FMapS :: Selector a b -> FMapS f a (f b)

instance (Select a) => GEq (FMapS f a) where
  FMapS sel1 `geq` FMapS sel2 =
    case sel1 `geq` sel2 of
      Just Refl -> Just Refl
      Nothing -> Nothing

instance (Select a) => GCompare (FMapS f a) where
  gcompare (FMapS sel1) (FMapS sel2) =
    case gcompare sel1 sel2 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT

instance (Functor f, Select a) => Select (FMap f a) where
  type Selector (FMap f a) = FMapS f a
  select (FMapS s) = unFMap . fmap (select s)

deriving via FMap Maybe a instance (Select a) => Select (Maybe a)

-- | The constant selector, which is useful for representing selectors into data structures where
-- all fields have the same type. Matching on a value of type @'ConstS' k a b@ causes @a@ and @b@ to
-- unify, effectively “pinning” @b@ to @a@.
data ConstS k a b where
  ConstS :: !k -> ConstS k a a

selectKey :: (Select a, Selector a ~ ConstS k v) => k -> a -> v
selectKey = select . ConstS

instance (Eq k) => GEq (ConstS k a) where
  ConstS a `geq` ConstS b
    | a == b = Just Refl
    | otherwise = Nothing

instance (Ord k) => GCompare (ConstS k a) where
  ConstS a `gcompare` ConstS b = case compare a b of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT

data FieldS r a where
  FieldS :: (KnownSymbol s, HasField s r a) => !(Proxy s) -> FieldS r a

instance (KnownSymbol s, HasField s r a) => IsLabel s (FieldS r a) where
  fromLabel = FieldS (Proxy @s)

instance GEq (FieldS r) where
  FieldS a `geq` FieldS b = case sameSymbol a b of
    -- If two fields of the same record have the same name, then their fields fundamentally must
    -- have the same type! However, unfortunately, `HasField` constraints use a functional
    -- dependency to enforce this rather than a type family, and functional dependencies don’t
    -- provide evidence, so we have to use `unsafeCoerce` here. Yuck!
    Just Refl -> Just (unsafeCoerce Refl)
    Nothing -> Nothing

instance GCompare (FieldS r) where
  FieldS a `gcompare` FieldS b = case sameSymbol a b of
    -- See note about `HasField` and `unsafeCoerce` above.
    Just Refl -> unsafeCoerce GEQ
    Nothing
      | symbolVal a < symbolVal b -> GLT
      | otherwise -> GGT

-- | A 'UniqueS' is, as the name implies, a globally-unique 'Selector', which can be created using
-- 'newUniqueS'. If a value of type @'UniqueS' a@ is found to be equal (via 'geq') with another
-- value of type @'UniqueS' b@, then @a@ and @b@ must be the same type. This effectively allows the
-- creation of a dynamically-extensible sum type, where new constructors can be created at runtime
-- using 'newUniqueS'.
type role UniqueS nominal

newtype UniqueS a = UniqueS Unique
  deriving (Eq)

newUniqueS :: (MonadIO m) => m (UniqueS a)
newUniqueS = UniqueS <$> liftIO newUnique
{-# INLINE newUniqueS #-}

instance GEq UniqueS where
  UniqueS a `geq` UniqueS b
    -- This use of `unsafeCoerce` is safe as long as we don’t export the constructor of `UniqueS`.
    -- Because a `UniqueS` is, in fact, unique, then we can be certain that equality of 'UniqueS's
    -- implies equality of their argument types.
    | a == b = Just (unsafeCoerce Refl)
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
    Nothing -> Nothing

instance (GCompare k) => GCompare (DMapS k f) where
  DMapS a `gcompare` DMapS b = case a `gcompare` b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
