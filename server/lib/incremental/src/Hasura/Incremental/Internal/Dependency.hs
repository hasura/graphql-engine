{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Supporting functionality for fine-grained dependency tracking.
module Hasura.Incremental.Internal.Dependency
  ( Access (AccessedAll),
    Accesses,
    unchanged,
    Dependency (..),
    DependencyKey (DependencyRoot),
    recordAccess,
    selectD,
    selectKeyD,
    selectMaybeD,
  )
where

import Data.Dependent.Map qualified as DM
import Data.Reflection
import Hasura.Incremental.Select
import Hasura.Prelude

-- | A 'Dependency' represents a value that a 'Rule' can /conditionally/ depend on. A 'Dependency'
-- is created using 'newDependency', and it can be “opened” again using 'dependOn'. What makes a
-- 'Dependency' useful is the way it cooperates with 'cache'---if a 'Dependency' is passed to a
-- cached rule, but that rule (or any of its sub-rules) never “opens” it using 'dependOn', then
-- subsequent executions of the rule will ignore the 'Dependency' when computing whether or not it
-- is necessary to re-execute the rule.
--
-- The above functionality is useful on its own to express conditional dependencies, but even more
-- useful is the ability to express /partial/ dependencies. For example, if a 'Dependency' contains
-- a 'HashMap', a rule can choose to only depend on the value associated with a particular key by
-- using 'selectKeyD' (or the more general 'selectD'). Only the parts that are actually used will be
-- counted when computing whether a rule needs to be re-executed.
data Dependency a = Dependency (DependencyKey a) a

-- | Applies a 'Selector' to select part of a 'Dependency'.
selectD :: (Select a) => Selector a b -> Dependency a -> Dependency b
selectD k (Dependency dk a) = Dependency (DependencyChild k dk) (select k a)

-- | Selects a single key from a dependency containing a map-like data structure.
selectKeyD :: (Select a, Selector a ~ ConstS k v) => k -> Dependency a -> Dependency v
selectKeyD = selectD . ConstS

selectMaybeD :: (Select a) => Selector a b -> Dependency (Maybe a) -> Dependency (Maybe b)
selectMaybeD = selectD . FMapS

-- | Tracks whether a 'Dependency' is a “root” dependency created by 'newDependency' or a “child”
-- dependency created from an existing dependency using 'selectD'.
data DependencyKey a where
  DependencyRoot :: UniqueS a -> DependencyKey a
  DependencyChild :: (Select a) => Selector a b -> DependencyKey a -> DependencyKey b

instance GEq DependencyKey where
  DependencyRoot a `geq` DependencyRoot b
    | Just Refl <- a `geq` b =
        Just Refl
  DependencyChild a1 a2 `geq` DependencyChild b1 b2
    | Just Refl <- a2 `geq` b2,
      Just Refl <- a1 `geq` b1 =
        Just Refl
  _ `geq` _ = Nothing

instance GCompare DependencyKey where
  DependencyRoot a `gcompare` DependencyRoot b = case gcompare a b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  DependencyChild a1 a2 `gcompare` DependencyChild b1 b2 = case gcompare a2 b2 of
    GLT -> GLT
    GEQ -> case gcompare a1 b1 of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT
  DependencyRoot _ `gcompare` DependencyChild _ _ = GLT
  DependencyChild _ _ `gcompare` DependencyRoot _ = GGT

-- | A mapping from root 'Dependency' keys to the accesses made against those dependencies.
newtype Accesses = Accesses {unAccesses :: DM.DMap UniqueS Access}

instance Semigroup Accesses where
  Accesses a <> Accesses b = Accesses $ DM.unionWithKey (const (<>)) a b

instance Monoid Accesses where
  mempty = Accesses DM.empty

recordAccess :: DependencyKey a -> Access a -> Accesses -> Accesses
recordAccess depKey !access (Accesses accesses) = case depKey of
  DependencyRoot rootKey -> Accesses $ DM.insertWith' (<>) rootKey access accesses
  DependencyChild selector parentKey ->
    recordAccess parentKey (AccessedParts $ DM.singleton selector access) (Accesses accesses)

unchanged :: ((Given Accesses) => Eq a) => Accesses -> a -> a -> Bool
unchanged accesses a b = give accesses (a == b)

-- | Records the accesses made within a single 'Dependency' and its children. The 'Semigroup'
-- instance for 'Access' computes a least upper bound:
--
--   * 'AccessedAll' serves as the top of the lattice and records the dependency’s entire value was
--     accessed.
--   * 'AccessedParts' records a set of accesses for individual parts of a dependency.
data Access a where
  AccessedAll :: (Eq a) => Access a
  AccessedParts :: (Select a) => DM.DMap (Selector a) Access -> Access a

instance Semigroup (Access a) where
  AccessedAll <> _ = AccessedAll
  _ <> AccessedAll = AccessedAll
  AccessedParts a <> AccessedParts b = AccessedParts $ DM.unionWithKey (const (<>)) a b

instance (Given Accesses, Eq a) => Eq (Dependency a) where
  Dependency key1 v1 == Dependency _ v2 =
    -- look up which parts of this dependency were previously accessed
    case lookupAccess key1 of
      -- looking up the access was enough to determine the result
      Left result -> result
      -- otherwise, look through the accessed children
      Right access -> unchangedBy v1 v2 access
    where
      -- Looks up the Access associated with the given DependencyKey, if it exists.
      lookupAccess :: DependencyKey b -> Either Bool (Access b)
      lookupAccess = \case
        DependencyRoot key -> handleNoAccess $ DM.lookup key (unAccesses given)
        DependencyChild selector key ->
          lookupAccess key >>= \case
            AccessedAll -> Left (v1 == v2)
            AccessedParts parts -> handleNoAccess $ DM.lookup selector parts
        where
          -- if this dependency was never accessed, then it’s certainly unchanged
          handleNoAccess = maybe (Left True) Right

      -- Walks the given values guided by the given Access, checking that all the subparts
      -- identified by the AccessedAll leaves are unchanged.
      unchangedBy :: forall b. b -> b -> Access b -> Bool
      unchangedBy a b = \case
        AccessedAll -> a == b
        AccessedParts parts -> DM.foldrWithKey reduce True parts
        where
          reduce :: (Select b) => Selector b c -> Access c -> Bool -> Bool
          reduce selector = (&&) . unchangedBy (select selector a) (select selector b)
