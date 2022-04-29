{-# LANGUAGE UndecidableInstances #-}

module Hasura.SQL.BackendMap
  ( BackendMap,
    empty,
    insert,
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
import Hasura.Prelude hiding (empty, lookup)
import Hasura.SQL.AnyBackend (AnyBackend, SatisfiesForAllBackends, dispatchAnyBackend, dispatchAnyBackend'', parseAnyBackendFromJSON, unpackAnyBackend)
import Hasura.SQL.Backend (BackendType, parseBackendTypeFromText)
import Hasura.SQL.Tag (HasTag, backendTag, reify)

-- | A BackendMap is a data structure that can contain at most one value of an 'i' per 'BackendType'
-- The 'i' type must be one that is parameterized by a BackendType-kinded type parameter
newtype BackendMap (i :: BackendType -> Type) = BackendMap (Map BackendType (AnyBackend i))
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance i `SatisfiesForAllBackends` Show => Show (BackendMap i)

deriving newtype instance i `SatisfiesForAllBackends` Eq => Eq (BackendMap i)

empty :: BackendMap i
empty = mempty

-- | Insert 'newValue' into the map, replacing any existing value that is of the same
-- 'BackendType' as represented by the particular 'AnyBackend' value passed.
insert :: AnyBackend i -> BackendMap i -> BackendMap i
insert newValue (BackendMap backendMap) =
  BackendMap $ Map.insert backendType newValue backendMap
  where
    backendType :: BackendType
    backendType = dispatchAnyBackend @HasTag newValue determineBackendType

    determineBackendType :: forall (b :: BackendType) i. HasTag b => i b -> BackendType
    determineBackendType _ = reify $ backendTag @b

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
