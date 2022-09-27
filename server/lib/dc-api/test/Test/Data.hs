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
    albumsRowsById,
    albumsTableRelationships,
    artistRelationshipName,
    tracksRelationshipName,
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
    -- = InvoiceLines table
    invoiceLinesTableName,
    invoiceLinesRows,
    -- = MediaTypes table
    mediaTypesTableName,
    mediaTypesRows,
    -- = Tracks table
    tracksTableName,
    tracksRows,
    tracksTableRelationships,
    invoiceLinesRelationshipName,
    mediaTypeRelationshipName,
    albumRelationshipName,
    genreRelationshipName,
    -- = Genres table
    genresTableName,
    genresRows,
    genresTableRelationships,
    -- = Utilities
    emptyQuery,
    sortBy,
    filterColumnsByQueryFields,
    filterColumns,
    renameColumns,
    onlyKeepRelationships,
    queryFields,
    responseRows,
    sortResponseRowsBy,
    responseAggregates,
    mkFieldsMap,
    insertField,
    deleteField,
    field,
    _ColumnFieldNumber,
    _ColumnFieldString,
    _ColumnFieldBoolean,
    columnField,
    queryComparisonColumn,
    currentComparisonColumn,
    orderByColumn,
  )
where

import Codec.Compression.GZip qualified as GZip
import Control.Arrow (first, (>>>))
import Control.Lens (Index, IxValue, Ixed, Traversal', ix, (%~), (&), (^.), (^..), (^?))
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson qualified as J
import Data.Aeson.Lens (_Bool, _Number, _String)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
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
schemaBS = $(makeRelativeToProject "test/Test/Data/schema-tables.json" >>= embedFile)

schemaTables :: [API.TableInfo]
schemaTables = sortOn API._tiName . either error id . eitherDecodeStrict $ schemaBS

numericColumns :: [API.ColumnName]
numericColumns = schemaTables >>= (API._tiColumns >>> mapMaybe (\API.ColumnInfo {..} -> if _ciType == API.NumberTy then Just _ciName else Nothing))

chinookXmlBS :: ByteString
chinookXmlBS = $(makeRelativeToProject "test/Test/Data/ChinookData.xml.gz" >>= embedFile)

chinookXml :: XML.Document
chinookXml = XML.parseLBS_ XML.def . GZip.decompress $ BSL.fromStrict chinookXmlBS

readTableFromXmlIntoRows :: API.TableName -> [HashMap API.FieldName API.FieldValue]
readTableFromXmlIntoRows tableName =
  rowToJsonObject <$> tableRows
  where
    tableNameToXmlTag :: API.TableName -> CI Text
    tableNameToXmlTag (API.TableName names) = CI.mk . Text.intercalate "_" $ NonEmpty.toList names

    tableRows :: [XML.Element]
    tableRows = chinookXml ^.. XML.root . XML.nodes . traverse . XML._Element . XML.named (tableNameToXmlTag tableName)

    rowToJsonObject :: XML.Element -> HashMap API.FieldName API.FieldValue
    rowToJsonObject element =
      let columnElements = element ^.. XML.nodes . traverse . XML._Element
          keyValuePairs = columnElementToProperty <$> columnElements
       in HashMap.fromList keyValuePairs

    columnElementToProperty :: XML.Element -> (API.FieldName, API.FieldValue)
    columnElementToProperty columnElement =
      let name = columnElement ^. XML.localName
          value = case columnElement ^. XML.nodes of
            [] -> API.mkColumnFieldValue $ J.Null
            _ ->
              let textValue = Text.concat $ columnElement ^.. XML.text
               in if API.ColumnName name `elem` numericColumns
                    then case eitherDecodeStrict $ Text.encodeUtf8 textValue of
                      Left _ -> API.mkColumnFieldValue $ J.String textValue
                      Right scientific -> API.mkColumnFieldValue $ J.Number scientific
                    else API.mkColumnFieldValue $ J.String textValue
       in (API.FieldName name, value)

mkTableName :: Text -> API.TableName
mkTableName = API.TableName . (:| [])

artistsTableName :: API.TableName
artistsTableName = mkTableName "Artist"

artistsRows :: [HashMap API.FieldName API.FieldValue]
artistsRows = sortBy (API.FieldName "ArtistId") $ readTableFromXmlIntoRows artistsTableName

artistsRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
artistsRowsById =
  HashMap.fromList $ mapMaybe (\artist -> (,artist) <$> artist ^? field "ArtistId" . _ColumnFieldNumber) artistsRows

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
albumsTableName = mkTableName "Album"

albumsRows :: [HashMap API.FieldName API.FieldValue]
albumsRows = sortBy (API.FieldName "AlbumId") $ readTableFromXmlIntoRows albumsTableName

albumsRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
albumsRowsById =
  HashMap.fromList $ mapMaybe (\album -> (,album) <$> album ^? field "AlbumId" . _ColumnFieldNumber) albumsRows

albumsTableRelationships :: API.TableRelationships
albumsTableRelationships =
  let artistsJoinFieldMapping = HashMap.fromList [(API.ColumnName "ArtistId", API.ColumnName "ArtistId")]
      tracksJoinFieldMapping = HashMap.fromList [(API.ColumnName "AlbumId", API.ColumnName "AlbumId")]
   in API.TableRelationships
        albumsTableName
        ( HashMap.fromList
            [ (artistRelationshipName, API.Relationship artistsTableName API.ObjectRelationship artistsJoinFieldMapping),
              (tracksRelationshipName, API.Relationship tracksTableName API.ArrayRelationship tracksJoinFieldMapping)
            ]
        )

artistRelationshipName :: API.RelationshipName
artistRelationshipName = API.RelationshipName "Artist"

tracksRelationshipName :: API.RelationshipName
tracksRelationshipName = API.RelationshipName "Tracks"

customersTableName :: API.TableName
customersTableName = mkTableName "Customer"

customersRows :: [HashMap API.FieldName API.FieldValue]
customersRows = sortBy (API.FieldName "CustomerId") $ readTableFromXmlIntoRows customersTableName

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
employeesTableName = mkTableName "Employee"

employeesRows :: [HashMap API.FieldName API.FieldValue]
employeesRows = sortBy (API.FieldName "EmployeeId") $ readTableFromXmlIntoRows employeesTableName

employeesRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
employeesRowsById =
  HashMap.fromList $ mapMaybe (\employee -> (,employee) <$> employee ^? field "EmployeeId" . _ColumnFieldNumber) employeesRows

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
invoicesTableName = mkTableName "Invoice"

invoicesRows :: [HashMap API.FieldName API.FieldValue]
invoicesRows = sortBy (API.FieldName "InvoiceId") $ readTableFromXmlIntoRows invoicesTableName

invoiceLinesTableName :: API.TableName
invoiceLinesTableName = mkTableName "InvoiceLine"

invoiceLinesRows :: [HashMap API.FieldName API.FieldValue]
invoiceLinesRows = sortBy (API.FieldName "InvoiceLineId") $ readTableFromXmlIntoRows invoiceLinesTableName

mediaTypesTableName :: API.TableName
mediaTypesTableName = mkTableName "MediaType"

mediaTypesRows :: [HashMap API.FieldName API.FieldValue]
mediaTypesRows = sortBy (API.FieldName "MediaTypeId") $ readTableFromXmlIntoRows mediaTypesTableName

tracksTableName :: API.TableName
tracksTableName = mkTableName "Track"

tracksRows :: [HashMap API.FieldName API.FieldValue]
tracksRows = sortBy (API.FieldName "TrackId") $ readTableFromXmlIntoRows tracksTableName

tracksTableRelationships :: API.TableRelationships
tracksTableRelationships =
  let invoiceLinesJoinFieldMapping = HashMap.fromList [(API.ColumnName "TrackId", API.ColumnName "TrackId")]
      mediaTypeJoinFieldMapping = HashMap.fromList [(API.ColumnName "MediaTypeId", API.ColumnName "MediaTypeId")]
      albumJoinFieldMapping = HashMap.fromList [(API.ColumnName "AlbumId", API.ColumnName "AlbumId")]
      genreJoinFieldMapping = HashMap.fromList [(API.ColumnName "GenreId", API.ColumnName "GenreId")]
   in API.TableRelationships
        tracksTableName
        ( HashMap.fromList
            [ (invoiceLinesRelationshipName, API.Relationship invoiceLinesTableName API.ArrayRelationship invoiceLinesJoinFieldMapping),
              (mediaTypeRelationshipName, API.Relationship mediaTypesTableName API.ObjectRelationship mediaTypeJoinFieldMapping),
              (albumRelationshipName, API.Relationship albumsTableName API.ObjectRelationship albumJoinFieldMapping),
              (genreRelationshipName, API.Relationship genresTableName API.ObjectRelationship genreJoinFieldMapping)
            ]
        )

invoiceLinesRelationshipName :: API.RelationshipName
invoiceLinesRelationshipName = API.RelationshipName "InvoiceLines"

mediaTypeRelationshipName :: API.RelationshipName
mediaTypeRelationshipName = API.RelationshipName "MediaType"

albumRelationshipName :: API.RelationshipName
albumRelationshipName = API.RelationshipName "Album"

genreRelationshipName :: API.RelationshipName
genreRelationshipName = API.RelationshipName "Genre"

genresTableName :: API.TableName
genresTableName = mkTableName "Genre"

genresRows :: [HashMap API.FieldName API.FieldValue]
genresRows = sortBy (API.FieldName "GenreId") $ readTableFromXmlIntoRows genresTableName

genresTableRelationships :: API.TableRelationships
genresTableRelationships =
  let joinFieldMapping = HashMap.fromList [(API.ColumnName "GenreId", API.ColumnName "GenreId")]
   in API.TableRelationships
        genresTableName
        ( HashMap.fromList
            [ (tracksRelationshipName, API.Relationship tracksTableName API.ArrayRelationship joinFieldMapping)
            ]
        )

emptyQuery :: API.Query
emptyQuery = API.Query Nothing Nothing Nothing Nothing Nothing Nothing

sortBy :: (Ixed m, Ord (IxValue m)) => Index m -> [m] -> [m]
sortBy propName = sortOn (^? ix propName)

filterColumnsByQueryFields :: API.Query -> HashMap API.FieldName API.FieldValue -> HashMap API.FieldName API.FieldValue
filterColumnsByQueryFields query =
  HashMap.filterWithKey (\key _value -> key `elem` columns)
  where
    columns = HashMap.keys $ queryFields query

filterColumns :: [Text] -> [HashMap API.FieldName API.FieldValue] -> [HashMap API.FieldName API.FieldValue]
filterColumns columns =
  fmap (HashMap.filterWithKey (\key _value -> key `elem` columns'))
  where
    columns' = API.FieldName <$> columns

renameColumns :: [(Text, Text)] -> [HashMap API.FieldName API.FieldValue] -> [HashMap API.FieldName API.FieldValue]
renameColumns columns =
  fmap (HashMap.fromList . fmap rename . HashMap.toList)
  where
    columns' = bimap API.FieldName API.FieldName <$> columns
    rename original@(key, value) =
      case find (\(k, _) -> k == key) columns' of
        Just (_, renamedKey) -> (renamedKey, value)
        Nothing -> original

onlyKeepRelationships :: [API.RelationshipName] -> API.TableRelationships -> API.TableRelationships
onlyKeepRelationships names tableRels =
  tableRels & API.trRelationships %~ HashMap.filterWithKey (\relName _ -> relName `elem` names)

queryFields :: API.Query -> HashMap API.FieldName API.Field
queryFields = fromMaybe mempty . API._qFields

responseRows :: API.QueryResponse -> [HashMap API.FieldName API.FieldValue]
responseRows = fromMaybe [] . API._qrRows

sortResponseRowsBy :: Text -> API.QueryResponse -> API.QueryResponse
sortResponseRowsBy columnName response = response & API.qrRows %~ (fmap (sortBy (API.FieldName columnName)))

responseAggregates :: API.QueryResponse -> HashMap API.FieldName J.Value
responseAggregates = fromMaybe mempty . API._qrAggregates

mkFieldsMap :: [(Text, v)] -> HashMap API.FieldName v
mkFieldsMap = HashMap.fromList . fmap (first API.FieldName)

insertField :: Text -> v -> HashMap API.FieldName v -> HashMap API.FieldName v
insertField fieldName = HashMap.insert (API.FieldName fieldName)

deleteField :: Text -> HashMap API.FieldName v -> HashMap API.FieldName v
deleteField fieldName = HashMap.delete (API.FieldName fieldName)

field :: (Ixed m, Index m ~ API.FieldName) => Text -> Traversal' m (IxValue m)
field fieldName = ix (API.FieldName fieldName)

_ColumnFieldNumber :: Traversal' API.FieldValue Scientific
_ColumnFieldNumber = API._ColumnFieldValue . _Number

_ColumnFieldString :: Traversal' API.FieldValue Text
_ColumnFieldString = API._ColumnFieldValue . _String

_ColumnFieldBoolean :: Traversal' API.FieldValue Bool
_ColumnFieldBoolean = API._ColumnFieldValue . _Bool

columnField :: Text -> API.Field
columnField = API.ColumnField . API.ColumnName

queryComparisonColumn :: Text -> API.ComparisonColumn
queryComparisonColumn columnName = API.ComparisonColumn API.QueryTable $ API.ColumnName columnName

currentComparisonColumn :: Text -> API.ComparisonColumn
currentComparisonColumn columnName = API.ComparisonColumn API.CurrentTable $ API.ColumnName columnName

orderByColumn :: [API.RelationshipName] -> Text -> API.OrderDirection -> API.OrderByElement
orderByColumn targetPath columnName orderDirection =
  API.OrderByElement targetPath (API.OrderByColumn $ API.ColumnName columnName) orderDirection
