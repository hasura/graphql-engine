{-# LANGUAGE UndecidableInstances #-}

module Hasura.SQL.BackendMap
  ( BackendMap,
    singleton,
    lookup,
    elems,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, withObject)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Extended (toTxt)
import Hasura.Prelude hiding (empty, lookup)
import Hasura.SQL.AnyBackend (AnyBackend, SatisfiesForAllBackends, dispatchAnyBackend'', mkAnyBackend, parseAnyBackendFromJSON, unpackAnyBackend)
import Hasura.SQL.Backend (BackendType, parseBackendTypeFromText)
import Hasura.SQL.Tag (HasTag, backendTag, reify)

-- | A BackendMap is a data structure that can contain at most one value of an 'i' per 'BackendType'
-- The 'i' type must be one that is parameterized by a BackendType-kinded type parameter
newtype BackendMap (i :: BackendType -> Type) = BackendMap (Map BackendType (AnyBackend i))
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance i `SatisfiesForAllBackends` Show => Show (BackendMap i)

deriving newtype instance i `SatisfiesForAllBackends` Eq => Eq (BackendMap i)

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

instance i `SatisfiesForAllBackends` FromJSON => FromJSON (BackendMap i) where
  parseJSON =
    withObject "BackendMap" $ \obj -> do
      BackendMap . Map.fromList
        <$> traverse
          ( \(backendTypeStr, val) -> do
              backendType <- parseBackendTypeFromText $ K.toText backendTypeStr
              (backendType,) <$> parseAnyBackendFromJSON backendType val
          )
          (KM.toList obj)

instance i `SatisfiesForAllBackends` ToJSON => ToJSON (BackendMap i) where
  toJSON (BackendMap backendMap) =
    object $ valueToPair <$> Map.elems backendMap
    where
      valueToPair :: AnyBackend i -> (K.Key, Value)
      valueToPair value = dispatchAnyBackend'' @ToJSON @HasTag value $ \(v :: i b) ->
        let backendTypeText = K.fromText . toTxt . reify $ backendTag @b
         in (backendTypeText, J.toJSON v)
