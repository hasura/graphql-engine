{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data
  ( schemaTables,
    artistsAsJson,
    artistsAsJsonById,
    albumsAsJson,
    customersAsJson,
    employeesAsJson,
    employeesAsJsonById,
    sortBy,
    filterColumnsByQueryFields,
    queryFields,
    responseRows,
    sortResponseRowsBy,
    _ColumnFieldNumber,
    _ColumnFieldString,
    _ColumnFieldBoolean,
  )
where

import Autodocodec.Extended (ValueWrapper (..), vwValue)
import Codec.Compression.GZip qualified as GZip
import Control.Lens (Index, IxValue, Ixed, Prism', ix, (%~), (&), (^.), (^..), (^?))
import Data.Aeson (Key, eitherDecodeStrict)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Hasura.Backends.DataConnector.API qualified as API
import Text.XML qualified as XML
import Text.XML.Lens qualified as XML
import Prelude

schemaBS :: ByteString
schemaBS = $(makeRelativeToProject "tests-dc-api/Test/Data/schema-tables.json" >>= embedFile)

schemaTables :: [API.TableInfo]
schemaTables = sortOn API.dtiName . either error id . eitherDecodeStrict $ schemaBS

chinookXmlBS :: ByteString
chinookXmlBS = $(makeRelativeToProject "tests-dc-api/Test/Data/ChinookData.xml.gz" >>= embedFile)

chinookXml :: XML.Document
chinookXml = XML.parseLBS_ XML.def . GZip.decompress $ BSL.fromStrict chinookXmlBS

readTableFromXmlIntoRows :: Text -> [KeyMap API.FieldValue]
readTableFromXmlIntoRows tableName =
  rowToJsonObject <$> tableRows
  where
    tableRows :: [XML.Element]
    tableRows = chinookXml ^.. XML.root . XML.nodes . traverse . XML._Element . XML.named (CI.mk tableName)

    rowToJsonObject :: XML.Element -> KeyMap API.FieldValue
    rowToJsonObject element =
      let columnElements = element ^.. XML.nodes . traverse . XML._Element
          keyValuePairs = columnElementToProperty <$> columnElements
       in KM.fromList keyValuePairs

    columnElementToProperty :: XML.Element -> (K.Key, API.FieldValue)
    columnElementToProperty columnElement =
      let name = K.fromText $ columnElement ^. XML.localName
          textValue = Text.concat $ columnElement ^.. XML.text
          value =
            case eitherDecodeStrict $ Text.encodeUtf8 textValue of
              Left _ -> API.ColumnFieldValue . ValueWrapper $ API.String textValue
              Right scientific -> API.ColumnFieldValue . ValueWrapper $ API.Number scientific
       in (name, value)

artistsAsJson :: [KeyMap API.FieldValue]
artistsAsJson = sortBy "ArtistId" $ readTableFromXmlIntoRows "Artist"

artistsAsJsonById :: HashMap Scientific (KeyMap API.FieldValue)
artistsAsJsonById =
  HashMap.fromList $ mapMaybe (\artist -> (,artist) <$> artist ^? ix "ArtistId" . _ColumnFieldNumber) artistsAsJson

albumsAsJson :: [KeyMap API.FieldValue]
albumsAsJson = sortBy "AlbumId" $ readTableFromXmlIntoRows "Album"

customersAsJson :: [KeyMap API.FieldValue]
customersAsJson = sortBy "CustomerId" $ readTableFromXmlIntoRows "Customer"

employeesAsJson :: [KeyMap API.FieldValue]
employeesAsJson = sortBy "EmployeeId" $ readTableFromXmlIntoRows "Employee"

employeesAsJsonById :: HashMap Scientific (KeyMap API.FieldValue)
employeesAsJsonById =
  HashMap.fromList $ mapMaybe (\employee -> (,employee) <$> employee ^? ix "EmployeeId" . _ColumnFieldNumber) employeesAsJson

sortBy :: (Ixed m, Ord (IxValue m)) => Index m -> [m] -> [m]
sortBy propName = sortOn (^? ix propName)

filterColumnsByQueryFields :: API.Query -> KeyMap API.FieldValue -> KeyMap API.FieldValue
filterColumnsByQueryFields query =
  KM.filterWithKey (\key _value -> key `elem` columns)
  where
    columns = KM.keys $ queryFields query

queryFields :: API.Query -> KeyMap API.Field
queryFields = fromMaybe mempty . API._qFields

responseRows :: API.QueryResponse -> [KeyMap API.FieldValue]
responseRows = fromMaybe [] . API._qrRows

sortResponseRowsBy :: Key -> API.QueryResponse -> API.QueryResponse
sortResponseRowsBy columnName response = response & API.qrRows %~ (fmap (sortBy columnName))

_ColumnFieldNumber :: Prism' API.FieldValue Scientific
_ColumnFieldNumber = API._ColumnFieldValue . vwValue . API._Number

_ColumnFieldString :: Prism' API.FieldValue Text
_ColumnFieldString = API._ColumnFieldValue . vwValue . API._String

_ColumnFieldBoolean :: Prism' API.FieldValue Bool
_ColumnFieldBoolean = API._ColumnFieldValue . vwValue . API._Boolean
