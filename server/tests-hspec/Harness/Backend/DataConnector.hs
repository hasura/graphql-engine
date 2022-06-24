{-# LANGUAGE QuasiQuotes #-}

-- | Data Connector helpers.
module Harness.Backend.DataConnector
  ( setup,
    teardown,
    defaultBackendConfig,
    defaultSourceMetadata,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (Value)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.Context (BackendType (DataConnector), defaultBackendTypeString, defaultSource)
import Harness.TestEnvironment (TestEnvironment)
import Prelude

--------------------------------------------------------------------------------

defaultSourceMetadata :: Value
defaultSourceMetadata =
  let source = defaultSource DataConnector
      backendType = defaultBackendTypeString DataConnector
   in [yaml|
name : *source
kind: *backendType
tables:
  - table: Album
    configuration:
      custom_root_fields:
        select: albums
        select_by_pk: albums_by_pk
      column_config:
        AlbumId:
          custom_name: id
        Title:
          custom_name: title
        ArtistId:
          custom_name: artist_id
    object_relationships:
      - name: artist
        using:
          manual_configuration:
            remote_table: Artist
            column_mapping:
              ArtistId: ArtistId
  - table: Artist
    configuration:
      custom_root_fields:
        select: artists
        select_by_pk: artists_by_pk
      column_config:
        ArtistId:
          custom_name: id
        Name:
          custom_name: name
    array_relationships:
      - name: albums
        using:
          manual_configuration:
            remote_table: Album
            column_mapping:
              ArtistId: ArtistId
configuration: {}
|]

defaultBackendConfig :: Value
defaultBackendConfig =
  let backendType = defaultBackendTypeString DataConnector
   in [yaml|
dataconnector:
  *backendType:
    uri: "http://127.0.0.1:65005/"
|]

-- | Setup the schema in the most expected way.
setup :: (TestEnvironment, ()) -> IO ()
setup (testEnvironment, _) = do
  -- Clear and reconfigure the metadata
  GraphqlEngine.setSource testEnvironment defaultSourceMetadata (Just defaultBackendConfig)

-- | Teardown the schema and tracking in the most expected way.
teardown :: (TestEnvironment, ()) -> IO ()
teardown (testEnvironment, _) = do
  GraphqlEngine.clearMetadata testEnvironment
