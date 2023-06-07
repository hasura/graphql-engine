{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}

module Data.GADT.Compare.Extended
  ( module Data.GADT.Compare,
    strengthenOrdering,
    extendGOrdering,
  )
where

import "dependent-sum" Data.GADT.Compare

strengthenOrdering :: Ordering -> GOrdering a a
strengthenOrdering LT = GLT
strengthenOrdering EQ = GEQ
strengthenOrdering GT = GGT

infixr 6 `extendGOrdering`

extendGOrdering :: GOrdering a b -> ((a ~ b) => GOrdering c d) -> GOrdering c d
extendGOrdering GLT _ = GLT
extendGOrdering GEQ x = x
extendGOrdering GGT _ = GGT
