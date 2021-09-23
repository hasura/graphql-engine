module Control.Lens.Extended
  ( module Control.Lens
  , (^..)
  , (^@..)
  ) where

import           Control.Lens hiding ((^..), (^@..))
import           Data.Monoid  (Endo)
import           GHC.Exts     (IsList, Item, fromList)

infixl 8 ^..
(^..) :: (IsList l, Item l ~ a) => s -> Getting (Endo [a]) s a -> l
v ^.. l = fromList (toListOf l v)
{-# INLINE (^..) #-}

infixl 8 ^@..
(^@..) :: (IsList l, Item l ~ (i, a)) => s -> IndexedGetting i (Endo [(i, a)]) s a -> l
v ^@.. l = fromList (itoListOf l v)
{-# INLINE (^@..) #-}
