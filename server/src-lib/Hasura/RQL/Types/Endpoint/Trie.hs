module Hasura.RQL.Types.Endpoint.Trie
  ( -- * Types
    MultiMapPathTrie,
    MatchResult (..),
    PathComponent (..),

    -- * Path matching
    matchPath,
    ambiguousPaths,
    ambiguousPathsGrouped,
  )
where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.Multi qualified as MM
import Data.Set qualified as S
import Data.Trie qualified as T
import Hasura.Prelude

-------------------------------------------------------------------------------
-- Types

-- | Trie from 'PathComponent's to 'MultiMap's
type MultiMapPathTrie a k v = T.Trie (PathComponent a) (MM.MultiMap k v)

-- | A component in a URL path: either a literal or a wildcard parameter
data PathComponent a
  = PathLiteral a
  | PathParam
  deriving stock (Show, Eq, Ord, Generic)

instance (ToJSON a) => ToJSON (PathComponent a)

instance (ToJSON a) => ToJSONKey (PathComponent a)

instance (Hashable a) => Hashable (PathComponent a)

-- | Result of matching a path @['PathComponent'] a@ and key @k@ in a 'MultiMapPathTrie'.
--
-- 'MatchResult' is a lattice where 'MatchNotFound' is the bottom element and
-- 'MatchAmbiguous' is the top element:
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
data MatchResult a k v
  = -- | Multiple results.
    MatchAmbiguous
  | -- | A single unambiguous result. Returns the value found and a list of
    -- parameter bindings.
    MatchFound v [a]
  | -- | A path was found, but not a key. Returns a list of keys found.
    MatchMissingKey (NonEmpty k)
  | -- | Path was not found in the 'MultiMapPathTrie'.
    MatchNotFound
  deriving stock (Eq, Show)

-- | Semigroup and Monoid instances implement join (i.e. least upper bound)
-- on the above lattice.
instance Semigroup (MatchResult a k v) where
  -- Ambiguous match with anything else is ambiguous
  MatchAmbiguous <> _ = MatchAmbiguous
  _ <> MatchAmbiguous = MatchAmbiguous
  -- Two unambiguous matches is ambiguous
  MatchFound {} <> MatchFound {} = MatchAmbiguous
  -- Unambiguous match with missing key or not found is unambiguous
  m@MatchFound {} <> _ = m
  _ <> m@MatchFound {} = m
  -- Collect allowed keys
  MatchMissingKey ks <> MatchMissingKey ks' = MatchMissingKey $ ks <> ks'
  -- Not found is the identity element
  MatchNotFound <> r = r
  r <> MatchNotFound = r

instance Monoid (MatchResult a k v) where
  mempty = MatchNotFound

-------------------------------------------------------------------------------
-- Matching paths

-- | Look up the value at a path.
-- @PathParam@ matches any path component.
-- Returns a list of pairs containing the value found and bindings for any @PathParam@s.
lookupPath :: (Hashable a) => [a] -> T.Trie (PathComponent a) v -> [(v, [a])]
lookupPath [] t = [(v, []) | v <- maybeToList (T.trieData t)]
lookupPath (x : xs) t = do
  (pc, t') <- matchPathComponent x $ T.trieMap t
  m <- lookupPath xs t'
  pure $ case pc of
    PathLiteral {} -> m
    PathParam -> (x :) <$> m
  where
    matchPathComponent ::
      (Hashable a) =>
      a ->
      HashMap.HashMap (PathComponent a) v ->
      [(PathComponent (), v)]
    matchPathComponent a m =
      catMaybes [(PathLiteral (),) <$> HashMap.lookup (PathLiteral a) m, (PathParam,) <$> HashMap.lookup PathParam m]

-- | Match a key @k@ and path @[a]@ against a @MultiMapPathTrie a k v@
matchPath :: (Hashable k, Hashable a) => k -> [a] -> MultiMapPathTrie a k v -> MatchResult a k v
matchPath k path = foldMap toResult . lookupPath path
  where
    toResult (methodMap, paramMatches) =
      case S.toList $ MM.lookup k methodMap of
        [] -> maybe MatchNotFound MatchMissingKey (nonEmpty $ MM.keys methodMap)
        [v] -> MatchFound v paramMatches
        _ -> MatchAmbiguous

-- | A version of ambiguousPaths that attempts to group all ambiguous paths that have overlapping endpoints
ambiguousPathsGrouped :: (Hashable a, Hashable k, Ord v, Ord a) => MultiMapPathTrie a k v -> [(S.Set [PathComponent a], S.Set v)]
ambiguousPathsGrouped = groupAmbiguousPaths . map (first S.singleton) . ambiguousPaths

groupAmbiguousPaths :: (Ord a, Ord v) => [(S.Set [PathComponent a], S.Set v)] -> [(S.Set [PathComponent a], S.Set v)]
groupAmbiguousPaths [] = []
groupAmbiguousPaths (x : xs) =
  if any fst added
    then groupAmbiguousPaths $ map snd added
    else x : groupAmbiguousPaths xs
  where
    added = map (add x) xs
    add (p1, v1) (p2, v2)
      | S.disjoint v1 v2 = (False, (p2, v2))
      | otherwise = (True, (S.union p1 p2, S.union v1 v2))

-- | Detect and return all ambiguous paths in the @MultiMapPathTrie@
-- A path @p@ is ambiguous if @matchPath k p@ can return @MatchAmbiguous@ for some @k@.
ambiguousPaths :: (Hashable a, Hashable k, Ord v) => MultiMapPathTrie a k v -> [([PathComponent a], S.Set v)]
ambiguousPaths (T.Trie pathMap methodMap) =
  thisNodeAmbiguousPaths ++ childNodesAmbiguousPaths
  where
    isAmbiguous e = S.size e >= 2
    ambiguous = mconcat $ filter isAmbiguous $ maybe [] MM.elems methodMap
    thisNodeAmbiguousPaths = guard (not $ null $ ambiguous) >> [([], ambiguous)]
    childNodesAmbiguousPaths = uncurry childNodeAmbiguousPaths =<< HashMap.toList pathMap
    childNodeAmbiguousPaths pc t = first (pc :) <$> ambiguousPaths (mergeWildcardTrie t)
    wildcardTrie = HashMap.lookup PathParam pathMap
    mergeWildcardTrie = maybe id (<>) wildcardTrie
