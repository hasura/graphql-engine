{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data
  ( Artist (..),
    Album (..),
    schemaTables,
    artists,
    artistsAsJson,
    artistsAsJsonById,
    albums,
    albumsAsJson,
    sortBy,
  )
where

import Control.Lens (ix, (^?))
import Data.Aeson (FromJSON (..), Object, eitherDecodeStrict, withObject, (.:))
import Data.Aeson.Lens (_Number)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)
import Hasura.Backends.DataWrapper.API (TableInfo (..))
import Prelude

data Artist = Artist
  { _artistId :: Int,
    _artistName :: Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \obj ->
    Artist
      <$> obj .: "id"
      <*> obj .: "name"

data Album = Album
  { _albumId :: Int,
    _albumTitle :: Text,
    _albumArtistId :: Int
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)

instance FromJSON Album where
  parseJSON = withObject "Album" $ \obj ->
    Album
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "artist_id"

schemaBS :: ByteString
schemaBS = $(makeRelativeToProject "tests-gdw-api/Test/Data/schema-tables.json" >>= embedFile)

artistsBS :: ByteString
artistsBS = $(makeRelativeToProject "tests-gdw-api/Test/Data/artists.json" >>= embedFile)

albumsBS :: ByteString
albumsBS = $(makeRelativeToProject "tests-gdw-api/Test/Data/albums.json" >>= embedFile)

schemaTables :: [TableInfo]
schemaTables = sortOn dtiName . either error id . eitherDecodeStrict $ schemaBS

artists :: [Artist]
artists = sortOn _artistId . either error id . eitherDecodeStrict $ artistsBS

artistsAsJson :: [Object]
artistsAsJson = sortBy "id" . either error id . eitherDecodeStrict $ artistsBS

artistsAsJsonById :: HashMap Scientific Object
artistsAsJsonById =
  HashMap.fromList $ mapMaybe (\artist -> (,artist) <$> artist ^? ix "id" . _Number) artistsAsJson

albums :: [Album]
albums = sortOn _albumId . either error id . eitherDecodeStrict $ albumsBS

albumsAsJson :: [Object]
albumsAsJson = sortBy "id" . either error id . eitherDecodeStrict $ albumsBS

sortBy :: Text -> [Object] -> [Object]
sortBy propName = sortOn (^? ix propName)
