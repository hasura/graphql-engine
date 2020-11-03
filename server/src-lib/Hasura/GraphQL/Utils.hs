module Hasura.GraphQL.Utils
  ( showName
  , showNamedTy
  , throwVE
  , getBaseTy
  , groupTuples
  , groupListWith
  , mkMapWith
  , showNames
  , unwrapTy
  , simpleGraphQLQuery
  , jsonValueToGValue
  ) where

import           Hasura.Prelude
import           Hasura.RQL.Types.Error

import           Data.Scientific               (floatingOrInteger)

import qualified Data.Aeson                    as A
import qualified Data.HashMap.Strict           as Map
import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

showName :: G.Name -> Text
showName name = "\"" <> G.unName name <> "\""

throwVE :: (MonadError QErr m) => Text -> m a
throwVE = throw400 ValidationFailed

showNamedTy :: G.NamedType -> Text
showNamedTy nt =
  "'" <> G.showNT nt <> "'"

getBaseTy :: G.GType -> G.NamedType
getBaseTy = \case
  G.TypeNamed _ n     -> n
  G.TypeList _ lt     -> getBaseTyL lt
  where
    getBaseTyL = getBaseTy . G.unListType

unwrapTy :: G.GType -> G.GType
unwrapTy =
  \case
    G.TypeList _ lt -> G.unListType lt
    nt -> nt

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

showNames :: (Foldable t) => t G.Name -> Text
showNames names =
  T.intercalate ", " $ map G.unName $ toList names

-- A simple graphql query to be used in generators
simpleGraphQLQuery :: Text
simpleGraphQLQuery = "query {author {id name}}"

-- | Convert a JSON value to a GraphQL value.
jsonValueToGValue :: A.Value -> G.Value
jsonValueToGValue = \case
  A.String t -> G.VString $ G.StringValue t
  -- TODO: Note the danger zone of scientific:
  A.Number n -> either (\(_::Float) -> G.VFloat n) G.VInt (floatingOrInteger n)
  A.Bool b -> G.VBoolean b
  A.Object o -> G.VObject $ G.ObjectValueG $
    map (uncurry G.ObjectFieldG . (G.Name *** jsonValueToGValue)) $ Map.toList o
  A.Array a -> G.VList $ G.ListValueG $ map jsonValueToGValue $ toList a
  A.Null -> G.VNull
