{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Endpoint
  ( EndpointName (..),
    EndpointMethod (..),
    EndpointUrl (),
    CreateEndpoint,
    EndpointDef (..),
    QueryReference (..),
    EndpointMetadata (..),
    DropEndpoint (..),
    module Trie,
    EndpointTrie,
    buildEndpointsTrie,
    qrCollectionName,
    qrQueryName,
    edQuery,
    ceComment,
    ceDefinition,
    ceMethods,
    ceName,
    ceUrl,
    deName,
    splitPath,
    mkEndpointUrl,
    unEndpointUrl,
  )
where

import Autodocodec (HasCodec (codec), dimapCodec, optionalField', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (boundedEnumCodec, typeableName)
import Control.Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.HashMap.Strict.Multi qualified as MM
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Data.Trie qualified as T
import Data.Typeable (Typeable)
import Hasura.Prelude
import Hasura.RQL.Types.Endpoint.Trie as Trie
import Hasura.RQL.Types.QueryCollection (CollectionName, QueryName)
import Web.HttpApiData (FromHttpApiData (..))

data EndpointMethod
  = GET
  | POST
  | PUT
  | DELETE
  | PATCH
  deriving
    (Show, Bounded, Enum, Eq, Ord, Hashable, FromJSON, ToJSON, ToJSONKey, Generic)

-- | JSON representations for each 'EndpointMethod' value
endpointMethodJsonString :: EndpointMethod -> String
endpointMethodJsonString = \case
  GET -> "GET"
  POST -> "POST"
  PUT -> "PUT"
  DELETE -> "DELETE"
  PATCH -> "PATCH"

instance HasCodec EndpointMethod where
  codec = boundedEnumCodec endpointMethodJsonString

instance ToTxt EndpointMethod where
  toTxt = tshow

newtype EndpointName = EndpointName {unEndpointName :: NonEmptyText}
  deriving newtype
    ( Show,
      Eq,
      Ord,
      ToTxt,
      Hashable,
      ToJSON,
      FromJSON
    )

instance HasCodec EndpointName where
  codec = dimapCodec EndpointName unEndpointName codec

newtype EndpointUrl = EndpointUrl {unEndpointUrl :: NonEmptyText}
  deriving newtype
    ( Show,
      Eq,
      Ord,
      ToTxt,
      Hashable,
      ToJSON,
      FromJSON
    )

instance HasCodec EndpointUrl where
  codec = dimapCodec EndpointUrl unEndpointUrl codec

mkEndpointUrl :: (ToTxt a) => a -> Maybe EndpointUrl
mkEndpointUrl s = EndpointUrl <$> mkNonEmptyText (toTxt s)

instance FromHttpApiData EndpointUrl where
  parseQueryParam s =
    parseQueryParam s >>= \t ->
      case mkNonEmptyText t of
        Nothing -> Left "Endpoint url must be non-empty"
        Just net -> Right (EndpointUrl net)

data QueryReference = QueryReference
  { _qrCollectionName :: CollectionName,
    _qrQueryName :: QueryName
  }
  deriving stock (Show, Eq, Generic)

instance HasCodec QueryReference where
  codec =
    AC.object "QueryReference"
      $ QueryReference
      <$> requiredField' "collection_name"
      AC..= _qrCollectionName
        <*> requiredField' "query_name"
      AC..= _qrQueryName

$(deriveJSON (aesonDrop 3 snakeCase) ''QueryReference)
$(makeLenses ''QueryReference)

data EndpointDef query = EndpointDef
  { _edQuery :: query
  }
  deriving stock (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (HasCodec query, Typeable query) => HasCodec (EndpointDef query) where
  codec =
    AC.object ("EndpointDef_" <> typeableName @query)
      $ EndpointDef
      <$> requiredField' "query"
      AC..= _edQuery

$(deriveJSON (aesonDrop 3 snakeCase) ''EndpointDef)
$(makeLenses ''EndpointDef)

type EndpointTrie query = MultiMapPathTrie Text EndpointMethod (EndpointMetadata query)

buildEndpointsTrie :: (Ord query) => [EndpointMetadata query] -> EndpointTrie query
buildEndpointsTrie = foldl' insert mempty
  where
    insert t q =
      let endpointMap = foldMap (`MM.singleton` q) $ _ceMethods q
       in T.insertWith (<>) (splitPath (const PathParam) PathLiteral (_ceUrl q)) endpointMap t

-- | Split a path and construct PathSegments based on callbacks for variables and literals
--   Var callback is passed the ":" prefix as part of the text.
splitPath :: (T.Text -> a) -> (T.Text -> a) -> EndpointUrl -> [a]
splitPath var lit = map toPathComponent . T.split (== '/') . toTxt
  where
    toPathComponent x
      | ":" `T.isPrefixOf` x = var x
      | otherwise = lit x

type CreateEndpoint = EndpointMetadata QueryReference

data EndpointMetadata query = EndpointMetadata
  { _ceName :: EndpointName,
    _ceUrl :: EndpointUrl,
    _ceMethods :: NonEmpty EndpointMethod, -- TODO: Use a set for this?
    _ceDefinition :: EndpointDef query,
    _ceComment :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (HasCodec query, Typeable query) => HasCodec (EndpointMetadata query) where
  codec =
    AC.object ("EndpointMetadata_" <> typeableName @query)
      $ EndpointMetadata
      <$> requiredField' "name"
      AC..= _ceName
        <*> requiredField' "url"
      AC..= _ceUrl
        <*> requiredField' "methods"
      AC..= _ceMethods
        <*> requiredField' "definition"
      AC..= _ceDefinition
        <*> optionalField' "comment"
      AC..= _ceComment

$(deriveJSON (aesonDrop 3 snakeCase) ''EndpointMetadata)
$(makeLenses ''EndpointMetadata)

data DropEndpoint = DropEndpoint
  { _deName :: EndpointName
  }
  deriving (Show, Eq, Generic)

$(deriveJSON (aesonDrop 3 snakeCase) ''DropEndpoint)
$(makeLenses ''DropEndpoint)
