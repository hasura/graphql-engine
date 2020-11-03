module Hasura.GraphQL.Utils
  ( showName
  , groupTuples
  , groupListWith
  , mkMapWith
  , showNames
  , simpleGraphQLQuery
  , getBaseTyWithNestedLevelsCount
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.List.NonEmpty            as NE
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.Text.Extended

showName :: G.Name -> Text
showName name = "\"" <> G.unName name <> "\""

getBaseTyWithNestedLevelsCount :: G.GType -> (G.Name, Int)
getBaseTyWithNestedLevelsCount ty = go ty 0
  where
    go :: G.GType -> Int -> (G.Name, Int)
    go gType ctr =
      case gType of
        G.TypeNamed _ n      -> (n, ctr)
        G.TypeList  _ gType' -> go gType' (ctr + 1)

groupListWith
  :: (Eq k, Hashable k, Foldable t, Functor t)
  => (v -> k) -> t v -> Map.HashMap k (NE.NonEmpty v)
groupListWith f l =
  groupTuples $ fmap (\v -> (f v, v)) l

groupTuples
  :: (Eq k, Hashable k, Foldable t)
  => t (k, v) -> Map.HashMap k (NE.NonEmpty v)
groupTuples =
  foldr groupFlds Map.empty
  where
    groupFlds (k, v) m = case Map.lookup k m of
      Nothing -> Map.insert k (v NE.:| []) m
      Just s  -> Map.insert k (v NE.<| s) m

-- either duplicate keys or the map
mkMapWith
  :: (Eq k, Hashable k, Foldable t, Functor t)
  => (v -> k) -> t v -> Either (NE.NonEmpty k) (Map.HashMap k v)
mkMapWith f l =
  case NE.nonEmpty dups of
    Just dupsNE -> Left dupsNE
    Nothing     -> Right $ Map.map NE.head mapG
  where
    mapG = groupListWith f l
    dups = Map.keys $ Map.filter ((> 1) . length) mapG

showNames :: (Functor t, Foldable t) => t G.Name -> Text
showNames = commaSeparated . fmap G.unName

-- A simple graphql query to be used in generators
simpleGraphQLQuery :: Text
simpleGraphQLQuery = "query {author {id name}}"
