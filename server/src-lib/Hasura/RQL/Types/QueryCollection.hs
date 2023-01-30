{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.QueryCollection
  ( CollectionName (..),
    CollectionDef (..),
    cdQueries,
    CreateCollection (..),
    ccName,
    ccDefinition,
    ccComment,
    RenameCollection (..),
    rcName,
    rcNewName,
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

import Autodocodec (HasCodec (..), bimapCodec, dimapCodec, optionalField', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (graphQLExecutableDocumentCodec)
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Language.GraphQL.Draft.Parser qualified as G
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
      PG.FromCol,
      PG.ToPrepArg,
      ToTxt,
      Generic
    )

instance HasCodec CollectionName where
  codec = dimapCodec CollectionName unCollectionName codec

newtype QueryName = QueryName {unQueryName :: NonEmptyText}
  deriving (Show, Eq, Ord, NFData, Hashable, ToJSON, ToJSONKey, FromJSON, ToTxt, Generic)

instance HasCodec QueryName where
  codec = dimapCodec QueryName unQueryName codec

newtype GQLQuery = GQLQuery {unGQLQuery :: G.ExecutableDocument G.Name}
  deriving (Show, Eq, Ord, NFData, Hashable, ToJSON, FromJSON)

instance HasCodec GQLQuery where
  codec = dimapCodec GQLQuery unGQLQuery graphQLExecutableDocumentCodec

newtype GQLQueryWithText
  = GQLQueryWithText (Text, GQLQuery)
  deriving (Show, Eq, Ord, NFData, Generic, Hashable)

instance HasCodec GQLQueryWithText where
  codec = bimapCodec dec enc $ codec @Text
    where
      dec t = mapLeft T.unpack $ GQLQueryWithText . (t,) . GQLQuery <$> G.parseExecutableDoc t
      enc (GQLQueryWithText (t, _)) = t

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
  deriving (Show, Eq, Ord, Generic)

instance NFData ListedQuery

instance Hashable ListedQuery

instance HasCodec ListedQuery where
  codec =
    AC.object "ListedQuery" $
      ListedQuery
        <$> requiredField' "name" AC..= _lqName
        <*> requiredField' "query" AC..= _lqQuery

$(deriveJSON hasuraJSON ''ListedQuery)

newtype CollectionDef = CollectionDef
  {_cdQueries :: [ListedQuery]}
  deriving (Show, Eq, Generic, NFData)

instance HasCodec CollectionDef where
  codec =
    AC.object "CollectionDef" $
      CollectionDef
        <$> requiredField' "queries" AC..= _cdQueries

$(deriveJSON hasuraJSON ''CollectionDef)
$(makeLenses ''CollectionDef)

data CreateCollection = CreateCollection
  { _ccName :: CollectionName,
    _ccDefinition :: CollectionDef,
    _ccComment :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec CreateCollection where
  codec =
    AC.object "CreateCollection" $
      CreateCollection
        <$> requiredField' "name" AC..= _ccName
        <*> requiredField' "definition" AC..= _ccDefinition
        <*> optionalField' "comment" AC..= _ccComment

$(deriveJSON hasuraJSON ''CreateCollection)
$(makeLenses ''CreateCollection)

collectionQueries :: CreateCollection -> [G.ExecutableDocument G.Name]
collectionQueries = map (unGQLQuery . getGQLQuery . _lqQuery) . _cdQueries . _ccDefinition

data RenameCollection = RenameCollection
  { _rcName :: CollectionName,
    _rcNewName :: CollectionName
  }
  deriving (Show, Eq, Generic)

$(deriveJSON hasuraJSON ''RenameCollection)
$(makeLenses ''RenameCollection)

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
