module Hasura.RQL.Types.Endpoint
  ( EndpointName(..)
  , EndpointMethod(..)
  , EndpointUrl()
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
  , splitPath
  , mkEndpointUrl
  ) where

import           Hasura.Prelude

import qualified Data.Text                        as T
import qualified Database.PG.Query                as Q

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text.Extended
import           Data.Text.NonEmpty
import           Web.HttpApiData                  (FromHttpApiData (..))

import           Hasura.RQL.Types.Endpoint.Trie   as Trie
import           Hasura.RQL.Types.QueryCollection (CollectionName, QueryName)


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

mkEndpointUrl :: ToTxt a => a -> Maybe EndpointUrl
mkEndpointUrl s = EndpointUrl <$> mkNonEmptyText (toTxt s)

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
      in insertPath (splitPath (const PathParam) PathLiteral (_ceUrl q)) endpointMap t

-- | Split a path and construct PathSegments based on callbacks for variables and literals
--   Var callback is passed the ":" prefix as part of the text.
splitPath :: (T.Text -> a) -> (T.Text -> a) -> EndpointUrl -> [a]
splitPath var lit = map toPathComponent . T.split (=='/') . toTxt
  where
  toPathComponent x
    | ":" `T.isPrefixOf` x = var x
    | otherwise            = lit x

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

