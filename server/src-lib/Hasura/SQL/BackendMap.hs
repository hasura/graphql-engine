{-# LANGUAGE UndecidableInstances #-}

module Hasura.SQL.BackendMap
  ( BackendMap,
    lookup,
    elems,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, withObject)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text.Extended (toTxt)
import Hasura.Prelude hiding (lookup)
import Hasura.SQL.AnyBackend (AnyBackend, SatisfiesForAllBackends, dispatchAnyBackend'', parseAnyBackendFromJSON, unpackAnyBackend)
import Hasura.SQL.Backend (BackendType, parseBackendTypeFromText)
import Hasura.SQL.Tag (HasTag, backendTag, reify)

-- | A BackendMap is a data structure that can contain at most one value of an 'i' per 'BackendType'
-- The 'i' type must be one that is parameterized by a BackendType-kinded type parameter
newtype BackendMap (i :: BackendType -> Type) = BackendMap (Map BackendType (AnyBackend i))
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance i `SatisfiesForAllBackends` Show => Show (BackendMap i)

deriving newtype instance i `SatisfiesForAllBackends` Eq => Eq (BackendMap i)

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
              backendType <- parseBackendTypeFromText backendTypeStr
              (backendType,) <$> parseAnyBackendFromJSON backendType val
          )
          (HashMap.toList obj)

instance i `SatisfiesForAllBackends` ToJSON => ToJSON (BackendMap i) where
  toJSON (BackendMap backendMap) =
    object $ valueToPair <$> Map.elems backendMap
    where
      valueToPair :: AnyBackend i -> (Text, Value)
      valueToPair value = dispatchAnyBackend'' @ToJSON @HasTag value $ \(v :: i b) ->
        let backendTypeText = toTxt . reify $ backendTag @b
         in (backendTypeText, J.toJSON v)
