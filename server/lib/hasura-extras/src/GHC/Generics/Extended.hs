module GHC.Generics.Extended
  ( module GHC.Generics,
    constrName,
  )
where

import Data.Kind (Type)
import GHC.Generics
import Prelude

-- | Return the constructor name of some data with a Generic instance, as a
-- string. Useful when constructing error or trace messages, or debugging.
constrName :: (HasConstructor (Rep a), Generic a) => a -> String
constrName a = genericConstrName $ from a
{-# INLINE constrName #-}

-- for constrName, see: https://stackoverflow.com/a/48179707/176841
class HasConstructor (f :: Type -> Type) where
  genericConstrName :: f x -> String

instance (HasConstructor f) => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x
  {-# INLINE genericConstrName #-}

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r
  {-# INLINE genericConstrName #-}

instance (Constructor c) => HasConstructor (C1 c f) where
  genericConstrName x = conName x
  {-# INLINE genericConstrName #-}
