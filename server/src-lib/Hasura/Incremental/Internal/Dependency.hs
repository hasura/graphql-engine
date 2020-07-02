{-# OPTIONS_HADDOCK not-home #-}

-- | Supporting functionality for fine-grained dependency tracking.
module Hasura.Incremental.Internal.Dependency where

import           Hasura.Prelude

import qualified Data.Dependent.Map            as DM
import qualified Data.URL.Template             as UT
import qualified Language.GraphQL.Draft.Syntax as G
import qualified Network.URI.Extended          as N

import           Data.Aeson                    (Value)
import           Data.CaseInsensitive          (CI)
import           Data.Functor.Classes          (Eq1 (..), Eq2 (..))
import           Data.GADT.Compare
import           Data.Int
import           Data.Scientific               (Scientific)
import           Data.Set                      (Set)
import           Data.Vector                   (Vector)
import           Data.Void                     (Void)
import           GHC.Generics                  ((:*:) (..), (:+:) (..), Generic (..), K1 (..),
                                                M1 (..), U1 (..), V1)

import           Hasura.Incremental.Select

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
data Dependency a = Dependency !(DependencyKey a) !a

instance (Eq a) => Eq (Dependency a) where
  Dependency _ a == Dependency _ b = a == b

-- | Applies a 'Selector' to select part of a 'Dependency'.
selectD :: (Select a) => Selector a b -> Dependency a -> Dependency b
selectD k (Dependency dk a) = Dependency (DependencyChild k dk) (select k a)

-- | Selects a single key from a dependency containing a map-like data structure.
selectKeyD :: (Select a, Selector a ~ ConstS k v) => k -> Dependency a -> Dependency v
selectKeyD = selectD . ConstS

-- | Tracks whether a 'Dependency' is a “root” dependency created by 'newDependency' or a “child”
-- dependency created from an existing dependency using 'selectD'.
data DependencyKey a where
  DependencyRoot :: !(UniqueS a) -> DependencyKey a
  DependencyChild :: (Select a) => !(Selector a b) -> !(DependencyKey a) -> DependencyKey b

instance GEq DependencyKey where
  DependencyRoot a `geq` DependencyRoot b
    | Just Refl <- a `geq` b
    = Just Refl
  DependencyChild a1 a2 `geq` DependencyChild b1 b2
    | Just Refl <- a2 `geq` b2
    , Just Refl <- a1 `geq` b1
    = Just Refl
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

-- | A typeclass that implements the dependency-checking machinery used by 'cache'. Morally, this
-- class is like 'Eq', but it only checks the parts of a 'Dependency' that were actually accessed on
-- the previous execution. It is highly unlikely you will need to implement any 'Cacheable'
-- instances yourself; the default implementation uses 'Generic' to derive an instance
-- automatically.
class (Eq a) => Cacheable a where
  unchanged :: Accesses -> a -> a -> Bool

  default unchanged :: (Generic a, GCacheable (Rep a)) => Accesses -> a -> a -> Bool
  unchanged accesses a b = gunchanged (from a) (from b) accesses
  {-# INLINABLE unchanged #-}

-- | A mapping from root 'Dependency' keys to the accesses made against those dependencies.
newtype Accesses = Accesses { unAccesses :: DM.DMap UniqueS Access }

instance Semigroup Accesses where
  Accesses a <> Accesses b = Accesses $ DM.unionWithKey (const (<>)) a b
instance Monoid Accesses where
  mempty = Accesses DM.empty

recordAccess :: DependencyKey a -> Access a -> Accesses -> Accesses
recordAccess depKey !access (Accesses accesses) = case depKey of
  DependencyRoot rootKey -> Accesses $ DM.insertWith' (<>) rootKey access accesses
  DependencyChild selector parentKey ->
    recordAccess parentKey (AccessedParts $ DM.singleton selector access) (Accesses accesses)

-- | Records the accesses made within a single 'Dependency' and its children. The 'Semigroup'
-- instance for 'Access' computes a least upper bound:
--
--   * 'AccessedAll' serves as the top of the lattice and records the dependency’s entire value was
--     accessed.
--   * 'AccessedParts' records a set of accesses for individual parts of a dependency.
data Access a where
  AccessedAll :: (Cacheable a) => Access a
  AccessedParts :: (Select a) => !(DM.DMap (Selector a) Access) -> Access a

instance Semigroup (Access a) where
  AccessedAll <> _ = AccessedAll
  _ <> AccessedAll = AccessedAll
  AccessedParts a <> AccessedParts b = AccessedParts $ DM.unionWithKey (const (<>)) a b

instance (Cacheable a) => Cacheable (Dependency a) where
  unchanged accesses (Dependency key1 v1) (Dependency _ v2) =
    -- look up which parts of this dependency were previously accessed
    case lookupAccess key1 of
      -- looking up the access was enough to determine the result
      Left result  -> result
      -- otherwise, look through the accessed children
      Right access -> unchangedBy v1 v2 access
    where
      -- Looks up the Access associated with the given DependencyKey, if it exists.
      lookupAccess :: DependencyKey b -> Either Bool (Access b)
      lookupAccess = \case
        DependencyRoot key -> handleNoAccess $ DM.lookup key (unAccesses accesses)
        DependencyChild selector key -> lookupAccess key >>= \case
          AccessedAll -> Left (unchanged accesses v1 v2)
          AccessedParts parts -> handleNoAccess $ DM.lookup selector parts
        where
          -- if this dependency was never accessed, then it’s certainly unchanged
          handleNoAccess = maybe (Left True) Right

      -- Walks the given values guided by the given Access, checking that all the subparts
      -- identified by the AccessedAll leaves are unchanged.
      unchangedBy :: forall b. b -> b -> Access b -> Bool
      unchangedBy a b = \case
        AccessedAll         -> unchanged accesses a b
        AccessedParts parts -> DM.foldrWithKey reduce True parts
        where
          reduce :: (Select b) => Selector b c -> Access c -> Bool -> Bool
          reduce selector = (&&) . unchangedBy (select selector a) (select selector b)

-- -------------------------------------------------------------------------------------------------
-- boilerplate Cacheable instances

instance Cacheable Char where unchanged _ = (==)
instance Cacheable Double where unchanged _ = (==)
instance Cacheable Int where unchanged _ = (==)
instance Cacheable Int32 where unchanged _ = (==)
instance Cacheable Integer where unchanged _ = (==)
instance Cacheable Scientific where unchanged _ = (==)
instance Cacheable Text where unchanged _ = (==)
instance Cacheable N.URIAuth where unchanged _ = (==)
instance Cacheable G.Name where unchanged _ = (==)

instance (Cacheable a) => Cacheable (Seq a) where
  unchanged = liftEq . unchanged
instance (Cacheable a) => Cacheable (Vector a) where
  unchanged = liftEq . unchanged
instance (Cacheable k, Cacheable v) => Cacheable (HashMap k v) where
  unchanged accesses = liftEq2 (unchanged accesses) (unchanged accesses)
instance (Cacheable a) => Cacheable (HashSet a) where
  unchanged = liftEq . unchanged
instance (Cacheable a) => Cacheable (Set a) where
  unchanged = liftEq . unchanged
instance (Cacheable a) => Cacheable (CI a) where
  unchanged _ = (==)


instance Cacheable ()
instance (Cacheable a, Cacheable b) => Cacheable (a, b)
instance (Cacheable a, Cacheable b, Cacheable c) => Cacheable (a, b, c)
instance (Cacheable a, Cacheable b, Cacheable c, Cacheable d) => Cacheable (a, b, c, d)
instance (Cacheable a, Cacheable b, Cacheable c, Cacheable d, Cacheable e) => Cacheable (a, b, c, d, e)

instance Cacheable Bool
instance Cacheable Void
instance Cacheable Value
instance Cacheable G.FragmentDefinition
instance Cacheable G.GType
instance Cacheable G.Nullability
instance Cacheable G.OperationType
instance Cacheable G.VariableDefinition
instance Cacheable G.InputValueDefinition
instance Cacheable G.EnumValueDefinition
instance Cacheable G.FieldDefinition
instance Cacheable G.ScalarTypeDefinition
instance Cacheable G.UnionTypeDefinition
instance Cacheable G.InterfaceTypeDefinition
instance Cacheable G.EnumTypeDefinition
instance Cacheable G.InputObjectTypeDefinition
instance Cacheable G.ObjectTypeDefinition
instance Cacheable G.TypeDefinition
instance Cacheable N.URI
instance Cacheable UT.Variable
instance Cacheable UT.TemplateItem
instance Cacheable UT.URLTemplate
instance (Cacheable a) => Cacheable (Maybe a)
instance (Cacheable a, Cacheable b) => Cacheable (Either a b)
instance Cacheable a => Cacheable [a]
instance Cacheable a => Cacheable (NonEmpty a)
instance Cacheable a => Cacheable (G.Directive a)
instance Cacheable a => Cacheable (G.ExecutableDefinition a)
instance (Cacheable (a b), Cacheable b) => Cacheable (G.Field a b)
instance Cacheable a => Cacheable (G.FragmentSpread a)
instance (Cacheable (a b), Cacheable b) => Cacheable (G.InlineFragment a b)
instance (Cacheable (a b), Cacheable b) => Cacheable (G.OperationDefinition a b)
instance (Cacheable (a b), Cacheable b) => Cacheable (G.Selection a b)
instance (Cacheable (a b), Cacheable b) => Cacheable (G.TypedOperationDefinition a b)
instance Cacheable G.Origin

instance Cacheable a => Cacheable (G.Value a)

deriving instance Cacheable G.Description
deriving instance Cacheable G.EnumValue
deriving instance Cacheable a => Cacheable (G.ExecutableDocument a)

deriving instance Generic G.SchemaDocument -- TODO: maybe add this in graphql-parser-hs?
instance Cacheable G.SchemaDocument

class GCacheable f where
  gunchanged :: f p -> f p -> Accesses -> Bool

instance GCacheable V1 where
  gunchanged a = case a of {}
  {-# INLINE gunchanged #-}

instance GCacheable U1 where
  gunchanged U1 U1 _ = True
  {-# INLINE gunchanged #-}

instance (Cacheable a) => GCacheable (K1 t a) where
  gunchanged (K1 a) (K1 b) accesses = unchanged accesses a b
  {-# INLINE gunchanged #-}

instance (GCacheable f) => GCacheable (M1 t m f) where
  gunchanged (M1 a) (M1 b) = gunchanged a b
  {-# INLINE gunchanged #-}

instance (GCacheable f, GCacheable g) => GCacheable (f :*: g) where
  gunchanged (a1 :*: a2) (b1 :*: b2) = liftA2 (&&) (gunchanged a1 b1) (gunchanged a2 b2)
  {-# INLINE gunchanged #-}

instance (GCacheable f, GCacheable g) => GCacheable (f :+: g) where
  gunchanged (L1 a) (L1 b) = gunchanged a b
  gunchanged (R1 a) (R1 b) = gunchanged a b
  gunchanged _      _      = const False
  {-# INLINE gunchanged #-}
