module Hasura.RQL.Types.Endpoint.Trie
  ( PathComponent(..)
  , Path
  , renderPath
  , Trie(..)
  , PathTrie
  , MatchResult(..)
  , MultiMap(..)
  , MultiMapTrie
  , singletonMultiMap
  , singletonTrie
  , insertPath
  , matchPath
  , ambiguousPaths
  , ambiguousPathsGrouped
  )
 where

import           Hasura.Prelude

import           Data.Aeson          (ToJSON, ToJSONKey)
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import           Data.Text.Extended  (ToTxt (..))
import           Test.QuickCheck     (scale)


-- | A component in a URL path: either a literal or a wildcard parameter
data PathComponent a = PathLiteral a | PathParam deriving (Show, Eq, Ord, Generic)

instance ToTxt a => ToTxt (PathComponent a) where
  toTxt (PathLiteral x) = toTxt x
  toTxt PathParam       = "*"

instance Arbitrary a => Arbitrary (PathComponent a) where
  arbitrary = genericArbitrary

type Path a = [PathComponent a]

renderPath :: ToTxt a => Path a -> Text
renderPath = T.intercalate "/" . fmap toTxt

instance ToJSON a => ToJSON (PathComponent a)
instance ToJSON a => ToJSONKey (PathComponent a)
instance Hashable a => Hashable (PathComponent a)

-- | Data structure for storing a value @v@ keyed on a sequence of @k@s
data Trie k v
  = Trie
  { _trieMap  :: M.HashMap k (Trie k v)
  , _trieData :: v
  }
  deriving (Eq, Show, Ord, Generic)

instance (ToJSONKey a, ToJSON v) => ToJSON (Trie a v) where

instance (Eq k, Hashable k, Arbitrary k, Eq v, Arbitrary v) => Arbitrary (Trie k v) where
    arbitrary = Trie <$> fmap M.fromList (scale (`div` 2) arbitrary) <*> arbitrary

instance (Eq k, Hashable k, Semigroup v) => Semigroup (Trie k v) where
  Trie m0 v0 <> Trie m1 v1 = Trie (M.unionWith (<>) m0 m1) (v0 <> v1)

instance  (Eq k, Hashable k, Monoid v) => Monoid (Trie k v) where
  mempty = Trie M.empty mempty

singletonTrie :: (Hashable k, Monoid v) => [k] -> v -> Trie k v
singletonTrie ps v = foldr (\p t -> Trie (M.singleton p t) mempty) (Trie M.empty v) ps

-- prop> insertPath ps = mappend . singletonTrie ps
insertPath :: (Eq a, Hashable a, Monoid v) => [a] -> v -> Trie a v -> Trie a v
insertPath [] v (Trie m v') = Trie m $ v <> v'
insertPath (p:ps) v (Trie m v') =
     Trie (M.alter (Just . maybe (singletonTrie ps v) (insertPath ps v)) p m) v'

type PathTrie a v = Trie (PathComponent a) v

-- | Look up the value at a path.
-- @PathParam@ matches any path component.
-- Returns a list of pairs containing the value found
-- and bindings for any @PathParam@s.
lookupPath :: (Eq a, Hashable a) => [a] -> PathTrie a v -> [(v, [a])]
lookupPath [] t = [(_trieData t, [])]
lookupPath (x:xs) t = do
    (pc, t') <- matchPathComponent x $ _trieMap t
    m <- lookupPath xs t'
    pure $ case pc of
        PathLiteral{} -> m
        PathParam     -> (x:) <$> m
    where
        matchPathComponent :: (Eq a, Hashable a) =>
            a -> M.HashMap (PathComponent a) v -> [(PathComponent (), v)]
        matchPathComponent a m =
            catMaybes [(PathLiteral (),) <$> M.lookup (PathLiteral a) m, (PathParam,) <$> M.lookup PathParam m]

-- | Map from keys to sets of values
newtype MultiMap k v
    = MultiMap
    { _unMultiMap :: HashMap k (S.Set v)
    } deriving (Eq, Show, ToJSON)

singletonMultiMap :: Hashable k => k -> v -> MultiMap k v
singletonMultiMap k v = MultiMap $ M.singleton k $ S.singleton v

instance (Eq k, Hashable k, Ord v) => Semigroup (MultiMap k v) where
    MultiMap m0 <> MultiMap m1 = MultiMap $ M.unionWith S.union m0 m1

instance (Eq k, Hashable k, Ord v) => Monoid (MultiMap k v) where
    mempty = MultiMap M.empty

instance (Eq k, Hashable k, Arbitrary k, Ord v, Arbitrary v) => Arbitrary (MultiMap k v) where
    arbitrary = MultiMap <$> fmap (M.fromList . take 5) arbitrary

-- | Trie from @PathComponents@ to @MultiMap@s
type MultiMapTrie a k v = PathTrie a (MultiMap k v)

-- | Result of matching a path @Path a@ and key @k@ in a @MultiMapTrie@
data MatchResult a k v
    = MatchAmbiguous -- ^ multiple results
    | MatchFound v [a] -- ^ a single unambiguous result. Returns value found and list of parameter bindings.
    | MatchMissingKey (NonEmpty k) -- ^ path was found, but not key. Returns list of keys found.
    | MatchNotFound -- ^ path was not found in the Trie
    deriving (Eq, Show)

-- @MatchResult@ is a lattice where @MatchNotFound@ is the bottom element and
-- @MatchAmbiguous@ is the top element:
--
--               MatchAmbiguous
--             /               \
--   MatchFound v0 as0     MatchFound v1 as1
--             \               /
--        MatchMissingKey (ks0 <> ks1)
--             /               \
--  MatchMissingKey ks0     MatchMissingKey ks1
--             \               /
--               MatchNotFound

-- | Semigroup and Monoid instances implement join (i.e. least upper bound)
-- on the above lattice.
instance Semigroup (MatchResult a k v) where
    -- Ambiguous match with anything else is ambiguous
  MatchAmbiguous <> _                       = MatchAmbiguous
  _ <> MatchAmbiguous                       = MatchAmbiguous

    -- Two unambiguous matches is ambiguous
  MatchFound{} <> MatchFound{}              = MatchAmbiguous

    -- Unambiguous match with missing key or not found is unambiguous
  m@MatchFound{} <> _                       = m
  _ <> m@MatchFound{}                       = m

    -- Collect allowed keys
  MatchMissingKey ks <> MatchMissingKey ks' = MatchMissingKey $ ks <> ks'

    -- Not found is the identity element
  MatchNotFound <> r                        = r
  r <> MatchNotFound                        = r

instance Monoid (MatchResult a k v) where
  mempty = MatchNotFound

-- | Match a key @k@ and path @[a]@ against a @MultiMapTrie a k v@
matchPath :: (Eq a, Eq k, Hashable k, Hashable a) => k -> [a] -> MultiMapTrie a k v -> MatchResult a k v
matchPath k path = foldMap toResult . lookupPath path
    where
        toResult (MultiMap methodMap, paramMatches) =
            case S.toList <$> M.lookup k methodMap of
                Just [v]   -> MatchFound v paramMatches
                Just (_:_) -> MatchAmbiguous
                _          -> maybe MatchNotFound MatchMissingKey (nonEmpty $ M.keys methodMap)

-- | A version of ambiguousPaths that attempts to group all ambiguous paths that have overlapping endpoints
ambiguousPathsGrouped :: (Hashable a, Eq k, Hashable k, Ord v, Ord a) => MultiMapTrie a k v -> [(S.Set (Path a), S.Set v)]
ambiguousPathsGrouped = groupAmbiguousPaths . map (first S.singleton) . ambiguousPaths

groupAmbiguousPaths :: (Ord a, Ord v) => [(S.Set (Path a), S.Set v)] -> [(S.Set (Path a), S.Set v)]
groupAmbiguousPaths [] = []
groupAmbiguousPaths (x:xs) =
  if any fst added
  then groupAmbiguousPaths $ map snd added
  else x : groupAmbiguousPaths xs
  where
  added = map (add x) xs
  add (p1, v1) (p2, v2)
    | intersects v1 v2 = (True,  (S.union p1 p2, S.union v1 v2))
    | otherwise        = (False, (p2, v2))

intersects :: Ord a => S.Set a -> S.Set a -> Bool
intersects a b = not $ S.null $ S.intersection a b

-- | Detect and return all ambiguous paths in the @MultiMapTrie@
-- A path @p@ is ambiguous if @matchPath k p@ can return @MatchAmbiguous@ for some @k@.
ambiguousPaths :: (Eq a, Hashable a, Eq k, Hashable k, Ord v) => MultiMapTrie a k v -> [(Path a, S.Set v)]
ambiguousPaths (Trie pathMap (MultiMap methodMap)) =
    thisNodeAmbiguousPaths ++ childNodesAmbiguousPaths
        where
            isAmbiguous e                = S.size e >= 2
            ambiguous                    = mconcat $ filter isAmbiguous $ M.elems methodMap
            thisNodeAmbiguousPaths       = guard (not $ null $ ambiguous) >> [([], ambiguous)]
            childNodesAmbiguousPaths     = uncurry childNodeAmbiguousPaths =<< M.toList pathMap
            childNodeAmbiguousPaths pc t = first (pc:) <$> ambiguousPaths (mergeWildcardTrie t)
            wildcardTrie                 = M.lookup PathParam pathMap
            mergeWildcardTrie            = maybe id (<>) wildcardTrie
