{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data
  ( schemaTables,
    artistsAsJson,
    artistsAsJsonById,
    albumsAsJson,
    sortBy,
  )
where

import Codec.Compression.GZip qualified as GZip
import Control.Lens (ix, (^.), (^..), (^?))
import Data.Aeson (Object, Value (..), eitherDecodeStrict)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (_Number)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasura.Backends.DataConnector.API (TableInfo (..))
import Text.XML qualified as XML
import Text.XML.Lens qualified as XML
import Prelude

schemaBS :: ByteString
schemaBS = $(makeRelativeToProject "tests-dc-api/Test/Data/schema-tables.json" >>= embedFile)

schemaTables :: [TableInfo]
schemaTables = sortOn dtiName . either error id . eitherDecodeStrict $ schemaBS

chinookXmlBS :: ByteString
chinookXmlBS = $(makeRelativeToProject "tests-dc-api/Test/Data/ChinookData.xml.gz" >>= embedFile)

chinookXml :: XML.Document
chinookXml = XML.parseLBS_ XML.def . GZip.decompress $ BSL.fromStrict chinookXmlBS

readTableFromXmlIntoJson :: Text -> [Object]
readTableFromXmlIntoJson tableName =
  rowToJsonObject <$> tableRows
  where
    tableRows :: [XML.Element]
    tableRows = chinookXml ^.. XML.root . XML.nodes . traverse . XML._Element . XML.named (CI.mk tableName)

    rowToJsonObject :: XML.Element -> Object
    rowToJsonObject element =
      let columnElements = element ^.. XML.nodes . traverse . XML._Element
          keyValuePairs = columnElementToProperty <$> columnElements
       in KM.fromList keyValuePairs

    columnElementToProperty :: XML.Element -> (K.Key, Value)
    columnElementToProperty columnElement =
      let name = K.fromText $ columnElement ^. XML.localName
          textValue = Text.concat $ columnElement ^.. XML.text
          value =
            case eitherDecodeStrict $ Text.encodeUtf8 textValue of
              Left _ -> String textValue
              Right scientific -> Number scientific
       in (name, value)

artistsAsJson :: [Object]
artistsAsJson = sortBy "ArtistId" $ readTableFromXmlIntoJson "Artist"

artistsAsJsonById :: HashMap Scientific Object
artistsAsJsonById =
  HashMap.fromList $ mapMaybe (\artist -> (,artist) <$> artist ^? ix "ArtistId" . _Number) artistsAsJson

albumsAsJson :: [Object]
albumsAsJson = sortBy "AlbumId" $ readTableFromXmlIntoJson "Album"

sortBy :: K.Key -> [Object] -> [Object]
sortBy propName = sortOn (^? ix propName)
