{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.QueryCollection
  ( CollectionName (..),
    CollectionDef (..),
    cdQueries,
    CreateCollection (..),
    ccName,
    ccDefinition,
    ccComment,
    AddQueryToCollection (..),
    DropQueryFromCollection (..),
    DropCollection (..),
    GQLQuery (..),
    GQLQueryWithText (..),
    QueryName (..),
    ListedQuery (..),
    getGQLQuery,
    getGQLQueryText,
    QueryCollections,
    collectionQueries,
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as Q
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

newtype CollectionName = CollectionName {unCollectionName :: NonEmptyText}
  deriving
    ( Show,
      Eq,
      Ord,
      Hashable,
      ToJSON,
      ToJSONKey,
      FromJSON,
      Q.FromCol,
      Q.ToPrepArg,
      ToTxt,
      Generic
    )

newtype QueryName = QueryName {unQueryName :: NonEmptyText}
  deriving (Show, Eq, Ord, NFData, Hashable, ToJSON, ToJSONKey, FromJSON, ToTxt, Generic, Cacheable)

newtype GQLQuery = GQLQuery {unGQLQuery :: G.ExecutableDocument G.Name}
  deriving (Show, Eq, Ord, NFData, Hashable, ToJSON, FromJSON, Cacheable)

newtype GQLQueryWithText
  = GQLQueryWithText (Text, GQLQuery)
  deriving (Show, Eq, Ord, NFData, Generic, Cacheable, Hashable)

instance FromJSON GQLQueryWithText where
  parseJSON v@(String t) = GQLQueryWithText . (t,) <$> parseJSON v
  parseJSON _ = fail "expecting String for GraphQL query"

instance ToJSON GQLQueryWithText where
  toJSON (GQLQueryWithText (t, _)) = String t

getGQLQuery :: GQLQueryWithText -> GQLQuery
getGQLQuery (GQLQueryWithText v) = snd v

getGQLQueryText :: GQLQueryWithText -> Text
getGQLQueryText (GQLQueryWithText v) = fst v

data ListedQuery = ListedQuery
  { _lqName :: QueryName,
    _lqQuery :: GQLQueryWithText
  }
  deriving (Show, Eq, Generic)

instance NFData ListedQuery

instance Cacheable ListedQuery

instance Hashable ListedQuery

$(deriveJSON hasuraJSON ''ListedQuery)

newtype CollectionDef = CollectionDef
  {_cdQueries :: [ListedQuery]}
  deriving (Show, Eq, Generic, NFData, Cacheable)

$(deriveJSON hasuraJSON ''CollectionDef)
$(makeLenses ''CollectionDef)

data CreateCollection = CreateCollection
  { _ccName :: CollectionName,
    _ccDefinition :: CollectionDef,
    _ccComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

$(deriveJSON hasuraJSON ''CreateCollection)
$(makeLenses ''CreateCollection)

collectionQueries :: CreateCollection -> [G.ExecutableDocument G.Name]
collectionQueries = map (unGQLQuery . getGQLQuery . _lqQuery) . _cdQueries . _ccDefinition

data DropCollection = DropCollection
  { _dcCollection :: CollectionName,
    _dcCascade :: Bool
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON ''DropCollection)

data AddQueryToCollection = AddQueryToCollection
  { _aqtcCollectionName :: CollectionName,
    _aqtcQueryName :: QueryName,
    _aqtcQuery :: GQLQueryWithText
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON ''AddQueryToCollection)

data DropQueryFromCollection = DropQueryFromCollection
  { _dqfcCollectionName :: CollectionName,
    _dqfcQueryName :: QueryName
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON ''DropQueryFromCollection)

type QueryCollections = InsOrdHashMap CollectionName CreateCollection
