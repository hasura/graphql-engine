module Hasura.SQL.Types
  ( ToSQL(..)
  , toSQLTxt
  , (<+>)
  )
where

import           Hasura.Prelude

import qualified Text.Builder      as TB

import           Instances.TH.Lift ()


class ToSQL a where
  toSQL :: a -> TB.Builder

instance ToSQL TB.Builder where
  toSQL x = x

instance (ToSQL a) => ToSQL (Maybe a) where
  toSQL (Just a) = toSQL a
  toSQL Nothing  = mempty

toSQLTxt :: (ToSQL a) => a -> Text
toSQLTxt = TB.run . toSQL


infixr 6 <+>
(<+>) :: (ToSQL a) => Text -> [a] -> TB.Builder
(<+>) _ [] = mempty
(<+>) kat (x:xs) =
  toSQL x <> mconcat [ TB.text kat <> toSQL x' | x' <- xs ]
{-# INLINE (<+>) #-}
