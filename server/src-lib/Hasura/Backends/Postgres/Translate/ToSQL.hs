module Hasura.Backends.Postgres.Translate.ToSQL where

import           Hasura.Prelude

import qualified Text.Builder     as TB

import           Hasura.SQL.Types


class ToSQL a where
  toSQL :: a -> TB.Builder

instance ToSQL TB.Builder where
  toSQL x = x

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing  = mempty

toSQLTxt :: (ToSQL a) => a -> Text
toSQLTxt = TB.run . toSQL


instance (ToSQL a) => ToSQL (CollectableType a) where
  toSQL = \case
    CollectableTypeScalar ty -> toSQL ty
    -- typename array is an sql standard way of declaring types
    CollectableTypeArray ty  -> toSQL ty <> " array"
