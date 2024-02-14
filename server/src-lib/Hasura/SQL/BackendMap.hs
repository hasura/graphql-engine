{-# LANGUAGE UndecidableInstances #-}

module Hasura.SQL.BackendMap
  ( BackendMap,
    singleton,
    lookup,
    lookupD,
    elems,
    alter,
    modify,
    overridesDeeply,
  )
where

--------------------------------------------------------------------------------

import Autodocodec (HasCodec (codec), ObjectCodec, optionalFieldWith')
import Autodocodec qualified as AC
import Autodocodec.Extended (typeableName)
import Data.Aeson qualified as J
import Data.Aeson.Extended
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Data
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (stripSuffix)
import Data.Text.Extended (toTxt)
import Hasura.Incremental.Internal.Dependency (Dependency (..), selectD)
import Hasura.Incremental.Select
import Hasura.Prelude hiding (empty, lookup, modify)
import Hasura.RQL.Types.BackendTag (BackendTag, HasTag, backendTag, reify)
import Hasura.RQL.Types.BackendType (BackendType (..), supportedBackends)
import Hasura.SQL.AnyBackend

--------------------------------------------------------------------------------

-- | A BackendMap is a data structure that can contain at most one value of an 'i' per 'BackendType'
-- The 'i' type must be one that is parameterized by a BackendType-kinded type parameter
newtype BackendMap (i :: BackendType -> Type) = BackendMap (Map BackendType (AnyBackend i))
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance (i `SatisfiesForAllBackends` Show) => Show (BackendMap i)

deriving newtype instance (i `SatisfiesForAllBackends` Eq) => Eq (BackendMap i)

instance
  ( i `SatisfiesForAllBackends` HasCodec,
    i `SatisfiesForAllBackends` Typeable
  ) =>
  HasCodec (BackendMap i)
  where
  codec =
    AC.object ("BackendMap_" <> objectNameSuffix)
      $ foldl'
        foldBackendType
        (pure mempty)
        supportedBackends
    where
      foldBackendType :: ObjectCodec (BackendMap i) (BackendMap i) -> BackendType -> ObjectCodec (BackendMap i) (BackendMap i)
      foldBackendType accum backendType = insertEntry backendType <$> accum <*> entryCodec backendType

      entryCodec :: BackendType -> ObjectCodec (BackendMap i) (Maybe (AnyBackend i))
      entryCodec backendType = optionalFieldWith' (toTxt backendType) (anyBackendCodec backendType) AC..= extractEntry backendType

      insertEntry :: BackendType -> BackendMap i -> Maybe (AnyBackend i) -> BackendMap i
      insertEntry backendType (BackendMap m) entry = case entry of
        Just v -> BackendMap $ Map.insert backendType v m
        Nothing -> BackendMap m

      extractEntry backendType (BackendMap m) = Map.lookup backendType m

      -- We need some distinguishing text for each instantiation of @i@.
      -- I don't know how to get that from a type with kind @BackendType -> Type@.
      -- So I'm applying @i@ to an arbitrary backend type, and attempting to
      -- remove the portion of generated text specific to that type.
      objectNameSuffix =
        let t = typeableName @(i 'DataConnector)
         in fromMaybe t $ stripSuffix "__DataConnector" t

instance (i `SatisfiesForAllBackends` FromJSON) => FromJSON (BackendMap i) where
  parseJSON =
    J.withObject "BackendMap" $ \obj -> do
      BackendMap
        . Map.fromList
        <$> traverse
          ( \keyValue -> do
              out <- parseJSONKeyValue keyValue
              pure $ (lowerTag out, out)
          )
          (KeyMap.toList obj)

instance (i `SatisfiesForAllBackends` ToJSON) => ToJSON (BackendMap i) where
  toJSON (BackendMap backendMap) =
    J.object $ valueToPair <$> Map.elems backendMap
    where
      valueToPair :: AnyBackend i -> (Key, J.Value)
      valueToPair value = dispatchAnyBackend'' @ToJSON @HasTag value $ \(v :: i b) ->
        let backendTypeText = Key.fromText . toTxt . reify $ backendTag @b
         in (backendTypeText, J.toJSON v)

instance Select (BackendMap i) where
  type Selector (BackendMap i) = BackendMapS i
  select (BackendMapS (_ :: BackendTag b)) = lookup @b

data BackendMapS i a where
  BackendMapS :: forall (b :: BackendType) (i :: BackendType -> Type). (HasTag b) => BackendTag b -> BackendMapS i (Maybe (i b))

instance GEq (BackendMapS i) where
  BackendMapS a `geq` BackendMapS b = case a `geq` b of
    Just Refl -> Just Refl
    Nothing -> Nothing

instance GCompare (BackendMapS i) where
  BackendMapS a `gcompare` BackendMapS b = case a `gcompare` b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT

lookupD ::
  forall (b :: BackendType) (i :: BackendType -> Type).
  (HasTag b) =>
  Dependency (BackendMap i) ->
  Dependency (Maybe (i b))
lookupD = selectD (BackendMapS (backendTag @b))

--------------------------------------------------------------------------------

singleton :: forall b i. (HasTag b) => i b -> BackendMap i
singleton value = BackendMap $ Map.singleton (reify $ backendTag @b) (mkAnyBackend value)

-- | Get a value from the map for the particular 'BackendType' 'b'. This function
-- is usually used with a type application.
-- @
-- lookup @('Postgres 'Vanilla) backendMap
-- @
lookup :: forall (b :: BackendType) i. (HasTag b) => BackendMap i -> Maybe (i b)
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
alter :: forall b i. (HasTag b) => (Maybe (i b) -> Maybe (i b)) -> BackendMap i -> BackendMap i
alter f (BackendMap bmap) = BackendMap $ Map.alter (wrap . f . unwrap) (reify @b backendTag) bmap
  where
    wrap :: Maybe (i b) -> Maybe (AnyBackend i)
    wrap = fmap mkAnyBackend

    unwrap :: Maybe (AnyBackend i) -> Maybe (i b)
    unwrap x = x >>= unpackAnyBackend @b

-- | The expression @a `overridesDeeply b@ applies the values from @a@ on top of the defaults @b@.
-- In practice this should union the maps for each backend type.
overridesDeeply :: (i `SatisfiesForAllBackends` Semigroup) => BackendMap i -> BackendMap i -> BackendMap i
overridesDeeply (BackendMap a) (BackendMap b) = BackendMap (Map.unionWith override a b)
  where
    override a' b' = mergeAnyBackend @Semigroup (<>) a' b' a'
