module Hasura.SQL.Types
  ( ToSQL (..),
    toSQLTxt,
    CollectableType (..),
    ExtensionsSchema (..),
  )
where

import Autodocodec (Autodocodec (..), HasCodec (codec), dimapCodec, named, textCodec)
import Data.Aeson
import Hasura.Prelude
import Text.Builder qualified as TB

class ToSQL a where
  toSQL :: a -> TB.Builder

instance ToSQL TB.Builder where
  toSQL x = x

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing = mempty

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

instance (FromJSON a) => FromJSON (CollectableType a) where
  parseJSON = genericParseJSON defaultOptions {constructorTagModifier = drop 6}

instance (ToJSON a) => ToJSON (CollectableType a) where
  toJSON = genericToJSON defaultOptions {constructorTagModifier = drop 6}
  toEncoding = genericToEncoding defaultOptions {constructorTagModifier = drop 6}

instance (ToSQL a) => ToSQL (CollectableType a) where
  toSQL = \case
    CollectableTypeScalar ty -> toSQL ty
    -- typename array is an sql standard way of declaring types
    CollectableTypeArray ty -> toSQL ty <> " array"

-- | The name of the schema in which the graphql-engine will install database extensions.
newtype ExtensionsSchema = ExtensionsSchema {_unExtensionsSchema :: Text}
  deriving (Show, Eq, Hashable, NFData)
  deriving (FromJSON, ToJSON) via (Autodocodec ExtensionsSchema)

instance HasCodec ExtensionsSchema where
  codec = named "ExtensionsSchema" $ dimapCodec ExtensionsSchema _unExtensionsSchema textCodec
