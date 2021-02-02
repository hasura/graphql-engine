module Hasura.RQL.Types.Endpoint
  ( EndpointName(..)
  , EndpointMethod(..)
  , CreateEndpoint
  , EndpointDef(..)
  , QueryReference(..)
  , EndpointMetadata(..)
  , DropEndpoint(..)
  , module Trie
  , EndpointTrie
  , buildEndpointsTrie
  , qrCollectionName
  , qrQueryName
  , edQuery
  , ceComment
  , ceDefinition
  , ceMethods
  , ceName
  , ceUrl
  , deName
  ) where

import           Hasura.Prelude

import qualified Database.PG.Query                as Q

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import qualified Data.Text                        as T
import           Data.Text.Extended
import           Data.Text.NonEmpty

import           Hasura.RQL.Instances             ()
import           Hasura.RQL.Types.Endpoint.Trie   as Trie
import           Hasura.RQL.Types.QueryCollection (CollectionName, QueryName)
import           Web.HttpApiData                  (FromHttpApiData (..))

newtype EndpointMethod = EndpointMethod { unEndpointMethod :: Text }
  deriving
  ( Show, Eq, Ord, Hashable, ToJSON , ToTxt, Generic, Arbitrary, ToJSONKey)

instance FromJSON EndpointMethod where
  -- TODO: Use a more representititve datatype
  parseJSON (String s) = do
    if s `elem` ["GET", "POST", "PUT", "DELETE", "PATCH"]
      then pure (EndpointMethod s)
      else fail $ "Method " <> T.unpack s <> " not supported."
  parseJSON _ = fail "Only standard HTTP methods supported"

newtype EndpointName = EndpointName { unEndpointName :: NonEmptyText }
  deriving ( Show, Eq, Ord, Hashable, ToJSON, ToJSONKey
           , FromJSON, FromJSONKey, Q.FromCol, Q.ToPrepArg, ToTxt
           , Generic, Arbitrary
           )

newtype EndpointUrl = EndpointUrl { unEndpointUrl :: NonEmptyText }
  deriving ( Show, Eq, Ord, Hashable, ToJSON, ToJSONKey
           , FromJSON, FromJSONKey, Q.FromCol, Q.ToPrepArg, ToTxt
           , Generic, Arbitrary
           )

instance FromHttpApiData EndpointUrl where
  parseQueryParam s = parseQueryParam s >>= \t ->
    case mkNonEmptyText t of
      Nothing  -> Left "Endpoint url must be non-empty"
      Just net -> Right (EndpointUrl net)

data QueryReference
  = QueryReference
  { _qrCollectionName :: !CollectionName
  , _qrQueryName      :: !QueryName
  } deriving stock (Show, Eq, Generic)
$(deriveJSON (aesonDrop 3 snakeCase) ''QueryReference)
$(makeLenses ''QueryReference)

data EndpointDef query
  = EndpointDef
  { _edQuery          :: !query
  } deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
$(deriveJSON (aesonDrop 3 snakeCase) ''EndpointDef)
$(makeLenses ''EndpointDef)

type EndpointTrie query = MultiMapTrie Text EndpointMethod (EndpointMetadata query)

buildEndpointsTrie :: Ord query => [EndpointMetadata query] -> EndpointTrie query
buildEndpointsTrie = foldl' insert mempty
  where
    insert t q =
      let endpointMap = foldMap (`singletonMultiMap` q) $ _ceMethods q
      in insertPath (split (_ceUrl q)) endpointMap t

    split :: EndpointUrl -> Path Text
    split = map toPathComponent . T.split (=='/') . toTxt

    toPathComponent :: T.Text -> PathComponent Text
    toPathComponent x
      | ":" `T.isPrefixOf` x = PathParam
      | otherwise            = PathLiteral x

type CreateEndpoint = EndpointMetadata QueryReference

data EndpointMetadata query
  = EndpointMetadata
  { _ceName       :: !EndpointName
  , _ceUrl        :: !EndpointUrl
  , _ceMethods    :: !(NonEmpty EndpointMethod) -- TODO: Use a set for this?
  , _ceDefinition :: !(EndpointDef query)
  , _ceComment    :: !(Maybe Text)
  } deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
$(deriveJSON (aesonDrop 3 snakeCase) ''EndpointMetadata)
$(makeLenses ''EndpointMetadata)

data DropEndpoint
  = DropEndpoint
  { _deName :: !EndpointName
  } deriving (Show, Eq, Generic)
$(deriveJSON (aesonDrop 3 snakeCase) ''DropEndpoint)
$(makeLenses ''DropEndpoint)

