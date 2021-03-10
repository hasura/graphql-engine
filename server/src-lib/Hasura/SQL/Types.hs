module Hasura.SQL.Types
  ( ToSQL(..)
  , toSQLTxt
  , CollectableType(..)
  )
where

import           Hasura.Prelude

import qualified Text.Builder       as TB

import           Data.Aeson
import           Data.Aeson.TH

import           Hasura.Incremental (Cacheable)


class ToSQL a where
  toSQL :: a -> TB.Builder

instance ToSQL TB.Builder where
  toSQL x = x

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing  = mempty

toSQLTxt :: (ToSQL a) => a -> Text
toSQLTxt = TB.run . toSQL

-- | The type of all Postgres types (i.e. scalars and arrays). This type is parameterized so that
-- we can have both @'CollectableType' 'PGScalarType'@ and @'CollectableType' 'Hasura.RQL.Types.PGColumnType'@, for
-- when we care about the distinction made by 'Hasura.RQL.Types.PGColumnType'. If we ever change
-- 'Hasura.RQL.Types.PGColumnType' to handle arrays, not just scalars, then the parameterization can
-- go away.
--
-- TODO (from master): This is incorrect modeling, as 'PGScalarType' will capture anything (under 'PGUnknown').
-- This should be fixed when support for all types is merged.
data CollectableType a
  = CollectableTypeScalar !a
  | CollectableTypeArray !a
  deriving (Show, Eq, Generic, Data, Functor)
instance (NFData a) => NFData (CollectableType a)
instance (Hashable a) => Hashable (CollectableType a)
instance (Cacheable a) => Cacheable (CollectableType a)
$(deriveJSON defaultOptions{constructorTagModifier = drop 6} ''CollectableType)

instance (ToSQL a) => ToSQL (CollectableType a) where
  toSQL = \case
    CollectableTypeScalar ty -> toSQL ty
    -- typename array is an sql standard way of declaring types
    CollectableTypeArray ty  -> toSQL ty <> " array"
