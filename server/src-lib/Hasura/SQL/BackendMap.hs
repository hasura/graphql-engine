{-# LANGUAGE UndecidableInstances #-}

module Hasura.SQL.BackendMap
  ( BackendMap,
    singleton,
    lookup,
    elems,
    alter,
    modify,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, Key, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Extended (toTxt)
import Hasura.Prelude hiding (empty, lookup, modify)
import Hasura.SQL.AnyBackend (AnyBackend, SatisfiesForAllBackends, dispatchAnyBackend'', mkAnyBackend, parseAnyBackendFromJSON, unpackAnyBackend)
import Hasura.SQL.Backend (BackendType, parseBackendTypeFromText)
import Hasura.SQL.Tag (HasTag, backendTag, reify)

--------------------------------------------------------------------------------

-- | A BackendMap is a data structure that can contain at most one value of an 'i' per 'BackendType'
-- The 'i' type must be one that is parameterized by a BackendType-kinded type parameter
newtype BackendMap (i :: BackendType -> Type) = BackendMap (Map BackendType (AnyBackend i))
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance i `SatisfiesForAllBackends` Show => Show (BackendMap i)

deriving newtype instance i `SatisfiesForAllBackends` Eq => Eq (BackendMap i)

instance i `SatisfiesForAllBackends` FromJSON => FromJSON (BackendMap i) where
  parseJSON =
    Aeson.withObject "BackendMap" $ \obj -> do
      BackendMap . Map.fromList
        <$> traverse
          ( \(backendTypeStr, val) -> do
              backendType <- parseBackendTypeFromText $ Key.toText backendTypeStr
              (backendType,) <$> parseAnyBackendFromJSON backendType val
          )
          (KeyMap.toList obj)

instance i `SatisfiesForAllBackends` ToJSON => ToJSON (BackendMap i) where
  toJSON (BackendMap backendMap) =
    Aeson.object $ valueToPair <$> Map.elems backendMap
    where
      valueToPair :: AnyBackend i -> (Key, Aeson.Value)
      valueToPair value = dispatchAnyBackend'' @ToJSON @HasTag value $ \(v :: i b) ->
        let backendTypeText = Key.fromText . toTxt . reify $ backendTag @b
         in (backendTypeText, Aeson.toJSON v)

--------------------------------------------------------------------------------

singleton :: forall b i. HasTag b => i b -> BackendMap i
singleton value = BackendMap $ Map.singleton (reify $ backendTag @b) (mkAnyBackend value)

-- | Get a value from the map for the particular 'BackendType' 'b'. This function
-- is usually used with a type application.
-- @
-- lookup @('Postgres 'Vanilla) backendMap
-- @
lookup :: forall (b :: BackendType) i. HasTag b => BackendMap i -> Maybe (i b)
lookup (BackendMap backendMap) =
  Map.lookup (reify $ backendTag @b) backendMap >>= unpackAnyBackend @b

-- | Get all values in the map
elems :: forall i. BackendMap i -> [AnyBackend i]
elems (BackendMap backendMap) = Map.elems backendMap

-- | The expression @modify f bmap@ alters the value @x@ at
-- @b@. @modify@ is a restricted version of 'alter' which cannot
-- delete entries and if there is no @b@ key present in the map, it
-- will apply the modification function to the @i b@ unit value and
-- insert the result at @b@.
modify :: forall b i. (HasTag b, Monoid (i b)) => (i b -> i b) -> BackendMap i -> BackendMap i
modify f = alter \case
  Nothing -> Just $ f mempty
  Just ab -> Just $ f ab

-- | The expression @alter f bmap@ alters the value @x@ at @b@, or
-- absence thereof. alter can be used to insert, delete, or update a
-- value in a Map.
--
-- In short : @lookup k (alter f k m) = f (lookup k m)@.
alter :: forall b i. HasTag b => (Maybe (i b) -> Maybe (i b)) -> BackendMap i -> BackendMap i
alter f (BackendMap bmap) = BackendMap $ Map.alter (wrap . f . unwrap) (reify @b backendTag) bmap
  where
    wrap :: Maybe (i b) -> Maybe (AnyBackend i)
    wrap = fmap mkAnyBackend

    unwrap :: Maybe (AnyBackend i) -> Maybe (i b)
    unwrap x = x >>= unpackAnyBackend @b
