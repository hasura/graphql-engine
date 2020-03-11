{-# LANGUAGE PolyKinds #-}

module Data.GADT.Compare.Extended
  ( module Data.GADT.Compare
  , strengthenOrdering
  ) where

import Prelude

import Data.GADT.Compare

strengthenOrdering :: Ordering -> GOrdering a a
strengthenOrdering LT = GLT
strengthenOrdering EQ = GEQ
strengthenOrdering GT = GGT
