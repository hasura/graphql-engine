{-# LANGUAGE TemplateHaskell #-}

module Hasura.Backends.DataConnector.Agent.Data
  ( Row (..),
    StaticData (..),
    schema,
    staticData,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromJust)
import Hasura.Backends.DataConnector.API qualified as API
import Prelude

--------------------------------------------------------------------------------

newtype Row = Row {unRow :: HashMap API.ColumnName API.Value}
  deriving stock (Show)
  deriving newtype (FromJSON)

newtype StaticData = StaticData (HashMap API.TableName [Row])
  deriving stock (Show)
  deriving newtype (FromJSON)

staticData :: StaticData
staticData = fromJust $ do
  albums <- J.decode $ BL.fromStrict $(makeRelativeToProject "src-dc-api/Hasura/Backends/DataConnector/Agent/Albums.json" >>= embedFile)
  artists <- J.decode $ BL.fromStrict $(makeRelativeToProject "src-dc-api/Hasura/Backends/DataConnector/Agent/Artists.json" >>= embedFile)
  pure $ StaticData $ Map.fromList [(API.TableName "albums", albums), (API.TableName "artists", artists)]

schema :: API.SchemaResponse
schema =
  API.SchemaResponse
    { API.srTables =
        [ API.TableInfo
            { API.dtiName = API.TableName "artists",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "id",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Artist primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "name",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The name of the artist"
                    }
                ],
              API.dtiPrimaryKey = Just "id",
              API.dtiDescription = Just "Collection of artists of music"
            },
          API.TableInfo
            { API.dtiName = API.TableName "albums",
              API.dtiColumns =
                [ API.ColumnInfo
                    { API.dciName = API.ColumnName "id",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "Album primary key identifier"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "title",
                      API.dciType = API.StringTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The title of the album"
                    },
                  API.ColumnInfo
                    { API.dciName = API.ColumnName "artist_id",
                      API.dciType = API.NumberTy,
                      API.dciNullable = False,
                      API.dciDescription = Just "The ID of the artist that created the album"
                    }
                ],
              API.dtiPrimaryKey = Just "id",
              API.dtiDescription = Just "Collection of music albums created by artists"
            }
        ]
    }
