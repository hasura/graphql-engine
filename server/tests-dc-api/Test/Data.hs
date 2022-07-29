{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data
  ( -- = Schema
    schemaTables,
    -- = Artists table
    artistsTableName,
    artistsRows,
    artistsRowsById,
    artistsTableRelationships,
    -- = Albums table
    albumsTableName,
    albumsRelationshipName,
    albumsRows,
    albumsTableRelationships,
    artistRelationshipName,
    -- = Customers table
    customersTableName,
    customersRows,
    customersTableRelationships,
    supportRepRelationshipName,
    -- = Employees table
    employeesTableName,
    employeesRows,
    employeesRowsById,
    employeesTableRelationships,
    supportRepForCustomersRelationshipName,
    -- = Invoices table
    invoicesTableName,
    invoicesRows,
    -- = Utilities
    emptyQuery,
    sortBy,
    filterColumnsByQueryFields,
    onlyKeepRelationships,
    queryFields,
    responseRows,
    sortResponseRowsBy,
    responseAggregates,
    _ColumnFieldNumber,
    _ColumnFieldString,
    _ColumnFieldBoolean,
    columnField,
    comparisonColumn,
    localComparisonColumn,
  )
where

import Codec.Compression.GZip qualified as GZip
import Control.Arrow ((>>>))
import Control.Lens (Index, IxValue, Ixed, Traversal', ix, (%~), (&), (^.), (^..), (^?))
import Data.Aeson (Key, eitherDecodeStrict)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (_Bool, _Number, _String)
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

numericColumns :: [API.ColumnName]
numericColumns = schemaTables >>= (API.dtiColumns >>> mapMaybe (\API.ColumnInfo {..} -> if dciType == API.NumberTy then Just dciName else Nothing))

chinookXmlBS :: ByteString
chinookXmlBS = $(makeRelativeToProject "tests-dc-api/Test/Data/ChinookData.xml.gz" >>= embedFile)

chinookXml :: XML.Document
chinookXml = XML.parseLBS_ XML.def . GZip.decompress $ BSL.fromStrict chinookXmlBS

readTableFromXmlIntoRows :: API.TableName -> [KeyMap API.FieldValue]
readTableFromXmlIntoRows (API.TableName tableName) =
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
          value = case columnElement ^. XML.nodes of
            [] -> API.mkColumnFieldValue $ J.Null
            _ ->
              let textValue = Text.concat $ columnElement ^.. XML.text
               in if API.ColumnName (K.toText name) `elem` numericColumns
                    then case eitherDecodeStrict $ Text.encodeUtf8 textValue of
                      Left _ -> API.mkColumnFieldValue $ J.String textValue
                      Right scientific -> API.mkColumnFieldValue $ J.Number scientific
                    else API.mkColumnFieldValue $ J.String textValue
       in (name, value)

artistsTableName :: API.TableName
artistsTableName = API.TableName "Artist"

artistsRows :: [KeyMap API.FieldValue]
artistsRows = sortBy "ArtistId" $ readTableFromXmlIntoRows artistsTableName

artistsRowsById :: HashMap Scientific (KeyMap API.FieldValue)
artistsRowsById =
  HashMap.fromList $ mapMaybe (\artist -> (,artist) <$> artist ^? ix "ArtistId" . _ColumnFieldNumber) artistsRows

albumsRelationshipName :: API.RelationshipName
albumsRelationshipName = API.RelationshipName "Albums"

artistsTableRelationships :: API.TableRelationships
artistsTableRelationships =
  let joinFieldMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
   in API.TableRelationships
        artistsTableName
        ( HashMap.fromList
            [ (albumsRelationshipName, API.Relationship albumsTableName API.ArrayRelationship joinFieldMapping)
            ]
        )

albumsTableName :: API.TableName
albumsTableName = API.TableName "Album"

albumsRows :: [KeyMap API.FieldValue]
albumsRows = sortBy "AlbumId" $ readTableFromXmlIntoRows albumsTableName

albumsTableRelationships :: API.TableRelationships
albumsTableRelationships =
  let joinFieldMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
   in API.TableRelationships
        albumsTableName
        ( HashMap.fromList
            [ (artistRelationshipName, API.Relationship artistsTableName API.ObjectRelationship joinFieldMapping)
            ]
        )

artistRelationshipName :: API.RelationshipName
artistRelationshipName = API.RelationshipName "Artist"

customersTableName :: API.TableName
customersTableName = API.TableName "Customer"

customersRows :: [KeyMap API.FieldValue]
customersRows = sortBy "CustomerId" $ readTableFromXmlIntoRows customersTableName

customersTableRelationships :: API.TableRelationships
customersTableRelationships =
  let joinFieldMapping = HashMap.fromList [(API.ColumnName "SupportRepId", API.ColumnName "EmployeeId")]
   in API.TableRelationships
        customersTableName
        ( HashMap.fromList
            [ (supportRepRelationshipName, API.Relationship employeesTableName API.ObjectRelationship joinFieldMapping)
            ]
        )

supportRepRelationshipName :: API.RelationshipName
supportRepRelationshipName = API.RelationshipName "SupportRep"

employeesTableName :: API.TableName
employeesTableName = API.TableName "Employee"

employeesRows :: [KeyMap API.FieldValue]
employeesRows = sortBy "EmployeeId" $ readTableFromXmlIntoRows employeesTableName

employeesRowsById :: HashMap Scientific (KeyMap API.FieldValue)
employeesRowsById =
  HashMap.fromList $ mapMaybe (\employee -> (,employee) <$> employee ^? ix "EmployeeId" . _ColumnFieldNumber) employeesRows

employeesTableRelationships :: API.TableRelationships
employeesTableRelationships =
  let joinFieldMapping = HashMap.fromList [(API.ColumnName "EmployeeId", API.ColumnName "SupportRepId")]
   in API.TableRelationships
        employeesTableName
        ( HashMap.fromList
            [ (supportRepForCustomersRelationshipName, API.Relationship customersTableName API.ArrayRelationship joinFieldMapping)
            ]
        )

supportRepForCustomersRelationshipName :: API.RelationshipName
supportRepForCustomersRelationshipName = API.RelationshipName "SupportRepForCustomers"

invoicesTableName :: API.TableName
invoicesTableName = API.TableName "Invoice"

invoicesRows :: [KeyMap API.FieldValue]
invoicesRows = sortBy "InvoiceId" $ readTableFromXmlIntoRows invoicesTableName

emptyQuery :: API.Query
emptyQuery = API.Query Nothing Nothing Nothing Nothing Nothing Nothing

sortBy :: (Ixed m, Ord (IxValue m)) => Index m -> [m] -> [m]
sortBy propName = sortOn (^? ix propName)

filterColumnsByQueryFields :: API.Query -> KeyMap API.FieldValue -> KeyMap API.FieldValue
filterColumnsByQueryFields query =
  KM.filterWithKey (\key _value -> key `elem` columns)
  where
    columns = KM.keys $ queryFields query

onlyKeepRelationships :: [API.RelationshipName] -> API.TableRelationships -> API.TableRelationships
onlyKeepRelationships names tableRels =
  tableRels & API.trRelationships %~ HashMap.filterWithKey (\relName _ -> relName `elem` names)

queryFields :: API.Query -> KeyMap API.Field
queryFields = fromMaybe mempty . API._qFields

responseRows :: API.QueryResponse -> [KeyMap API.FieldValue]
responseRows = fromMaybe [] . API._qrRows

sortResponseRowsBy :: Key -> API.QueryResponse -> API.QueryResponse
sortResponseRowsBy columnName response = response & API.qrRows %~ (fmap (sortBy columnName))

responseAggregates :: API.QueryResponse -> KeyMap API.Value
responseAggregates = fromMaybe mempty . API._qrAggregates

_ColumnFieldNumber :: Traversal' API.FieldValue Scientific
_ColumnFieldNumber = API._ColumnFieldValue . _Number

_ColumnFieldString :: Traversal' API.FieldValue Text
_ColumnFieldString = API._ColumnFieldValue . _String

_ColumnFieldBoolean :: Traversal' API.FieldValue Bool
_ColumnFieldBoolean = API._ColumnFieldValue . _Bool

columnField :: Text -> API.Field
columnField = API.ColumnField . API.ColumnName

comparisonColumn :: [API.RelationshipName] -> Text -> API.ComparisonColumn
comparisonColumn path columnName = API.ComparisonColumn path $ API.ColumnName columnName

localComparisonColumn :: Text -> API.ComparisonColumn
localComparisonColumn columnName = comparisonColumn [] columnName
