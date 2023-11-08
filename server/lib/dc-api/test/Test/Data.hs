{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data
  ( -- = Chinook Test Data
    TestData (..),
    mkTestData,
    schemaTables,
    allTableRows,
    -- = TestingEdgeCases Test Data
    EdgeCasesTestData (..),
    mkEdgeCasesTestData,
    -- = Functions Test Data
    FunctionsTestData (..),
    mkFunctionsTestData,
    -- = Utilities
    emptyQuery,
    emptyMutationRequest,
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
    fieldAt,
    _ColumnFieldNumber,
    _ColumnFieldString,
    _ColumnFieldBoolean,
    _ColumnFieldNull,
    _RelationshipFieldRows,
    scalarValueComparison,
    orderByColumn,
    insertAutoIncPk,
    autoIncPks,
    mkSubqueryFieldValue,
    mkSubqueryRowsFieldValue,
    mkSubqueryAggregatesFieldValue,
    mkAndExpr,
    mkOrExpr,
  )
where

import Codec.Compression.GZip qualified as GZip
import Command (NameCasing (..), TestConfig (..))
import Control.Arrow (first, (>>>))
import Control.Lens (At, Index, IxValue, Ixed, Traversal', at, ix, (%~), (&), (?~), (^.), (^..), (^?), _Just)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson qualified as J
import Data.Aeson.Lens (_Bool, _Null, _Number, _String)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (Scientific)
import Data.Set qualified as Set
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
schemaTables = either error id . eitherDecodeStrict $ schemaBS

numericColumns :: [API.ColumnName]
numericColumns =
  schemaTables
    >>= ( API._tiColumns
            >>> mapMaybe
              ( \API.ColumnInfo {..} ->
                  if _ciType == API.ColumnTypeScalar (API.ScalarType "number")
                    then Just _ciName
                    else Nothing
              )
        )

edgeCasesSchemaBS :: ByteString
edgeCasesSchemaBS = $(makeRelativeToProject "test/Test/Data/edge-cases-schema-tables.json" >>= embedFile)

edgeCasesSchemaTables :: [API.TableInfo]
edgeCasesSchemaTables = either error id . eitherDecodeStrict $ edgeCasesSchemaBS

chinookXmlBS :: ByteString
chinookXmlBS = $(makeRelativeToProject "test/Test/Data/Chinook.xml.gz" >>= embedFile)

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
  let joinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
   in API.TableRelationships
        artistsTableName
        ( HashMap.fromList
            [ (albumsRelationshipName, API.Relationship (API.TTargetTable albumsTableName) API.ArrayRelationship joinFieldMapping)
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
  let artistsJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ArtistId", API.mkColumnSelector $ API.ColumnName "ArtistId")]
      tracksJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "AlbumId", API.mkColumnSelector $ API.ColumnName "AlbumId")]
   in API.TableRelationships
        albumsTableName
        ( HashMap.fromList
            [ (artistRelationshipName, API.Relationship (API.TTargetTable artistsTableName) API.ObjectRelationship artistsJoinFieldMapping),
              (tracksRelationshipName, API.Relationship (API.TTargetTable tracksTableName) API.ArrayRelationship tracksJoinFieldMapping)
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

customersRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
customersRowsById =
  HashMap.fromList $ mapMaybe (\customer -> (,customer) <$> customer ^? field "CustomerId" . _ColumnFieldNumber) customersRows

customersTableRelationships :: API.TableRelationships
customersTableRelationships =
  let supportRepJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "SupportRepId", API.mkColumnSelector $ API.ColumnName "EmployeeId")]
      invoicesJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "CustomerId", API.mkColumnSelector $ API.ColumnName "CustomerId")]
   in API.TableRelationships
        customersTableName
        ( HashMap.fromList
            [ (supportRepRelationshipName, API.Relationship (API.TTargetTable employeesTableName) API.ObjectRelationship supportRepJoinFieldMapping),
              (invoicesRelationshipName, API.Relationship (API.TTargetTable invoicesTableName) API.ArrayRelationship invoicesJoinFieldMapping)
            ]
        )

supportRepRelationshipName :: API.RelationshipName
supportRepRelationshipName = API.RelationshipName "SupportRep"

invoicesRelationshipName :: API.RelationshipName
invoicesRelationshipName = API.RelationshipName "Invoices"

employeesTableName :: API.TableName
employeesTableName = mkTableName "Employee"

employeesRows :: [HashMap API.FieldName API.FieldValue]
employeesRows = sortBy (API.FieldName "EmployeeId") $ readTableFromXmlIntoRows employeesTableName

employeesRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
employeesRowsById =
  HashMap.fromList $ mapMaybe (\employee -> (,employee) <$> employee ^? field "EmployeeId" . _ColumnFieldNumber) employeesRows

employeesTableRelationships :: API.TableRelationships
employeesTableRelationships =
  let supportRepJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "EmployeeId", API.mkColumnSelector $ API.ColumnName "SupportRepId")]
      reportsToEmployeeJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "ReportsTo", API.mkColumnSelector $ API.ColumnName "EmployeeId")]
   in API.TableRelationships
        employeesTableName
        ( HashMap.fromList
            [ (supportRepForCustomersRelationshipName, API.Relationship (API.TTargetTable customersTableName) API.ArrayRelationship supportRepJoinFieldMapping),
              (reportsToEmployeeRelationshipName, API.Relationship (API.TTargetTable employeesTableName) API.ObjectRelationship reportsToEmployeeJoinFieldMapping)
            ]
        )

supportRepForCustomersRelationshipName :: API.RelationshipName
supportRepForCustomersRelationshipName = API.RelationshipName "SupportRepForCustomers"

reportsToEmployeeRelationshipName :: API.RelationshipName
reportsToEmployeeRelationshipName = API.RelationshipName "ReportsToEmployee"

invoicesTableName :: API.TableName
invoicesTableName = mkTableName "Invoice"

invoicesRows :: [HashMap API.FieldName API.FieldValue]
invoicesRows = sortBy (API.FieldName "InvoiceId") $ readTableFromXmlIntoRows invoicesTableName

invoicesRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
invoicesRowsById =
  HashMap.fromList $ mapMaybe (\invoice -> (,invoice) <$> invoice ^? field "InvoiceId" . _ColumnFieldNumber) invoicesRows

invoicesTableRelationships :: API.TableRelationships
invoicesTableRelationships =
  let invoiceLinesJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "InvoiceId", API.mkColumnSelector $ API.ColumnName "InvoiceId")]
      customersJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "CustomerId", API.mkColumnSelector $ API.ColumnName "CustomerId")]
   in API.TableRelationships
        invoicesTableName
        ( HashMap.fromList
            [ (invoiceLinesRelationshipName, API.Relationship (API.TTargetTable invoiceLinesTableName) API.ArrayRelationship invoiceLinesJoinFieldMapping),
              (customerRelationshipName, API.Relationship (API.TTargetTable customersTableName) API.ObjectRelationship customersJoinFieldMapping)
            ]
        )

customerRelationshipName :: API.RelationshipName
customerRelationshipName = API.RelationshipName "Customer"

invoiceLinesTableName :: API.TableName
invoiceLinesTableName = mkTableName "InvoiceLine"

invoiceLinesRows :: [HashMap API.FieldName API.FieldValue]
invoiceLinesRows = sortBy (API.FieldName "InvoiceLineId") $ readTableFromXmlIntoRows invoiceLinesTableName

invoiceLinesTableRelationships :: API.TableRelationships
invoiceLinesTableRelationships =
  let invoiceJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "InvoiceId", API.mkColumnSelector $ API.ColumnName "InvoiceId")]
      tracksJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "TrackId", API.mkColumnSelector $ API.ColumnName "TrackId")]
   in API.TableRelationships
        invoiceLinesTableName
        ( HashMap.fromList
            [ (invoiceRelationshipName, API.Relationship (API.TTargetTable invoicesTableName) API.ObjectRelationship invoiceJoinFieldMapping),
              (trackRelationshipName, API.Relationship (API.TTargetTable tracksTableName) API.ObjectRelationship tracksJoinFieldMapping)
            ]
        )

invoiceRelationshipName :: API.RelationshipName
invoiceRelationshipName = API.RelationshipName "Invoice"

trackRelationshipName :: API.RelationshipName
trackRelationshipName = API.RelationshipName "Track"

mediaTypesTableName :: API.TableName
mediaTypesTableName = mkTableName "MediaType"

mediaTypesRows :: [HashMap API.FieldName API.FieldValue]
mediaTypesRows = sortBy (API.FieldName "MediaTypeId") $ readTableFromXmlIntoRows mediaTypesTableName

tracksTableName :: API.TableName
tracksTableName = mkTableName "Track"

tracksRows :: [HashMap API.FieldName API.FieldValue]
tracksRows = sortBy (API.FieldName "TrackId") $ readTableFromXmlIntoRows tracksTableName

tracksRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue)
tracksRowsById =
  HashMap.fromList $ mapMaybe (\track -> (,track) <$> track ^? field "TrackId" . _ColumnFieldNumber) tracksRows

tracksTableRelationships :: API.TableRelationships
tracksTableRelationships =
  let invoiceLinesJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "TrackId", API.mkColumnSelector $ API.ColumnName "TrackId")]
      mediaTypeJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "MediaTypeId", API.mkColumnSelector $ API.ColumnName "MediaTypeId")]
      albumJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "AlbumId", API.mkColumnSelector $ API.ColumnName "AlbumId")]
      genreJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "GenreId", API.mkColumnSelector $ API.ColumnName "GenreId")]
      playlistTracksJoinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "TrackId", API.mkColumnSelector $ API.ColumnName "TrackId")]
   in API.TableRelationships
        tracksTableName
        ( HashMap.fromList
            [ (invoiceLinesRelationshipName, API.Relationship (API.TTargetTable invoiceLinesTableName) API.ArrayRelationship invoiceLinesJoinFieldMapping),
              (mediaTypeRelationshipName, API.Relationship (API.TTargetTable mediaTypesTableName) API.ObjectRelationship mediaTypeJoinFieldMapping),
              (albumRelationshipName, API.Relationship (API.TTargetTable albumsTableName) API.ObjectRelationship albumJoinFieldMapping),
              (genreRelationshipName, API.Relationship (API.TTargetTable genresTableName) API.ObjectRelationship genreJoinFieldMapping),
              (playlistTracksRelationshipName, API.Relationship (API.TTargetTable playlistTracksTableName) API.ArrayRelationship playlistTracksJoinFieldMapping)
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

playlistTracksRelationshipName :: API.RelationshipName
playlistTracksRelationshipName = API.RelationshipName "PlaylistTracks"

genresTableName :: API.TableName
genresTableName = mkTableName "Genre"

genresRows :: [HashMap API.FieldName API.FieldValue]
genresRows = sortBy (API.FieldName "GenreId") $ readTableFromXmlIntoRows genresTableName

mkFibonacciRows :: Int -> [HashMap API.FieldName API.FieldValue]
mkFibonacciRows n = take n $ fibonacciRow <$> fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    fibonacciRow x = HashMap.singleton (API.FieldName "Value") (API.mkColumnFieldValue (J.Number x))

genresTableRelationships :: API.TableRelationships
genresTableRelationships =
  let joinFieldMapping = API.ColumnPathMapping $ HashMap.fromList [(API.mkColumnSelector $ API.ColumnName "GenreId", API.mkColumnSelector $ API.ColumnName "GenreId")]
   in API.TableRelationships
        genresTableName
        ( HashMap.fromList
            [ (tracksRelationshipName, API.Relationship (API.TTargetTable tracksTableName) API.ArrayRelationship joinFieldMapping)
            ]
        )

playlistsTableName :: API.TableName
playlistsTableName = mkTableName "Playlist"

playlistsRows :: [HashMap API.FieldName API.FieldValue]
playlistsRows = sortBy (API.FieldName "PlaylistId") $ readTableFromXmlIntoRows playlistsTableName

playlistTracksTableName :: API.TableName
playlistTracksTableName = mkTableName "PlaylistTrack"

playlistTracksRows :: [HashMap API.FieldName API.FieldValue]
playlistTracksRows = sortOn (\r -> (r ^? ix (API.FieldName "PlaylistId"), r ^? ix (API.FieldName "TrackId"))) $ readTableFromXmlIntoRows playlistTracksTableName

allTableRows :: HashMap API.TableName ([HashMap API.FieldName API.FieldValue])
allTableRows =
  HashMap.fromList
    [ (artistsTableName, artistsRows),
      (albumsTableName, albumsRows),
      (customersTableName, customersRows),
      (employeesTableName, employeesRows),
      (genresTableName, genresRows),
      (invoicesTableName, invoicesRows),
      (invoiceLinesTableName, invoiceLinesRows),
      (mediaTypesTableName, mediaTypesRows),
      (playlistsTableName, playlistsRows),
      (playlistTracksTableName, playlistTracksRows),
      (tracksTableName, tracksRows)
    ]

data TestData = TestData
  { -- = Schema
    _tdSchemaTables :: [API.TableInfo],
    -- = Artists table
    _tdArtistsTableName :: API.TableName,
    _tdArtistsRows :: [HashMap API.FieldName API.FieldValue],
    _tdArtistsRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue),
    _tdArtistsTableRelationships :: API.TableRelationships,
    _tdAlbumsRelationshipName :: API.RelationshipName,
    -- = Albums table
    _tdAlbumsTableName :: API.TableName,
    _tdAlbumsRows :: [HashMap API.FieldName API.FieldValue],
    _tdAlbumsRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue),
    _tdAlbumsTableRelationships :: API.TableRelationships,
    _tdArtistRelationshipName :: API.RelationshipName,
    _tdTracksRelationshipName :: API.RelationshipName,
    -- = Customers table
    _tdCustomersTableName :: API.TableName,
    _tdCustomersRows :: [HashMap API.FieldName API.FieldValue],
    _tdCustomersRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue),
    _tdCustomersTableRelationships :: API.TableRelationships,
    _tdInvoicesRelationshipName :: API.RelationshipName,
    _tdSupportRepRelationshipName :: API.RelationshipName,
    -- = Employees table
    _tdEmployeesTableName :: API.TableName,
    _tdEmployeesRows :: [HashMap API.FieldName API.FieldValue],
    _tdEmployeesRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue),
    _tdEmployeesTableRelationships :: API.TableRelationships,
    _tdSupportRepForCustomersRelationshipName :: API.RelationshipName,
    _tdReportsToEmployeeRelationshipName :: API.RelationshipName,
    -- = Invoices table
    _tdInvoicesTableName :: API.TableName,
    _tdInvoicesRows :: [HashMap API.FieldName API.FieldValue],
    _tdInvoicesRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue),
    _tdInvoicesTableRelationships :: API.TableRelationships,
    _tdCustomerRelationshipName :: API.RelationshipName,
    -- = InvoiceLines table
    _tdInvoiceLinesTableName :: API.TableName,
    _tdInvoiceLinesRows :: [HashMap API.FieldName API.FieldValue],
    _tdInvoiceLinesTableRelationships :: API.TableRelationships,
    _tdInvoiceRelationshipName :: API.RelationshipName,
    _tdTrackRelationshipName :: API.RelationshipName,
    -- = MediaTypes table
    _tdMediaTypesTableName :: API.TableName,
    _tdMediaTypesRows :: [HashMap API.FieldName API.FieldValue],
    -- = Tracks table
    _tdTracksTableName :: API.TableName,
    _tdTracksRows :: [HashMap API.FieldName API.FieldValue],
    _tdTracksRowsById :: HashMap Scientific (HashMap API.FieldName API.FieldValue),
    _tdTracksTableRelationships :: API.TableRelationships,
    _tdInvoiceLinesRelationshipName :: API.RelationshipName,
    _tdMediaTypeRelationshipName :: API.RelationshipName,
    _tdAlbumRelationshipName :: API.RelationshipName,
    _tdGenreRelationshipName :: API.RelationshipName,
    _tdPlaylistTracksRelationshipName :: API.RelationshipName,
    -- = PlaylistTracks table
    _tdPlaylistTracksTableName :: API.TableName,
    _tdPlaylistTracksRows :: [HashMap API.FieldName API.FieldValue],
    -- = Genres table
    _tdGenresTableName :: API.TableName,
    _tdGenresRows :: [HashMap API.FieldName API.FieldValue],
    _tdGenresTableRelationships :: API.TableRelationships,
    -- = Scalar Types
    _tdFindColumnScalarType :: API.TableName -> Text -> API.ScalarType,
    -- = Utility functions
    _tdColumnName :: Text -> API.ColumnName,
    _tdColumnField :: API.TableName -> Text -> API.Field,
    _tdMkDefaultTableInsertSchema :: API.TableName -> API.TableInsertSchema,
    _tdColumnInsertSchema :: API.TableName -> Text -> API.ColumnInsertSchema,
    _tdRowColumnOperatorValue :: API.TableName -> Text -> J.Value -> API.RowColumnOperatorValue,
    _tdQueryComparisonColumn :: Text -> API.ScalarType -> API.ComparisonColumn,
    _tdCurrentComparisonColumn :: Text -> API.ScalarType -> API.ComparisonColumn,
    _tdOrderByColumn :: [API.RelationshipName] -> Text -> API.OrderDirection -> API.OrderByElement
  }

-- | Test data from the Chinook dataset template
mkTestData :: API.SchemaResponse -> TestConfig -> TestData
mkTestData schemaResponse testConfig =
  TestData
    { _tdSchemaTables = formatTableInfo testConfig <$> schemaTables,
      _tdArtistsTableName = formatTableName testConfig artistsTableName,
      _tdArtistsRows = artistsRows,
      _tdArtistsRowsById = artistsRowsById,
      _tdArtistsTableRelationships = formatTableRelationships artistsTableRelationships,
      _tdAlbumsRelationshipName = albumsRelationshipName,
      _tdAlbumsTableName = formatTableName testConfig albumsTableName,
      _tdAlbumsRows = albumsRows,
      _tdAlbumsRowsById = albumsRowsById,
      _tdAlbumsTableRelationships = formatTableRelationships albumsTableRelationships,
      _tdArtistRelationshipName = artistRelationshipName,
      _tdTracksRelationshipName = tracksRelationshipName,
      _tdCustomersTableName = formatTableName testConfig customersTableName,
      _tdCustomersRows = customersRows,
      _tdCustomersRowsById = customersRowsById,
      _tdCustomersTableRelationships = formatTableRelationships customersTableRelationships,
      _tdInvoicesRelationshipName = invoicesRelationshipName,
      _tdSupportRepRelationshipName = supportRepRelationshipName,
      _tdEmployeesTableName = formatTableName testConfig employeesTableName,
      _tdEmployeesRows = employeesRows,
      _tdEmployeesRowsById = employeesRowsById,
      _tdEmployeesTableRelationships = formatTableRelationships employeesTableRelationships,
      _tdSupportRepForCustomersRelationshipName = supportRepForCustomersRelationshipName,
      _tdReportsToEmployeeRelationshipName = reportsToEmployeeRelationshipName,
      _tdInvoicesTableName = formatTableName testConfig invoicesTableName,
      _tdInvoicesRows = invoicesRows,
      _tdInvoicesRowsById = invoicesRowsById,
      _tdInvoicesTableRelationships = formatTableRelationships invoicesTableRelationships,
      _tdCustomerRelationshipName = customerRelationshipName,
      _tdInvoiceLinesTableName = formatTableName testConfig invoiceLinesTableName,
      _tdInvoiceLinesRows = invoiceLinesRows,
      _tdInvoiceLinesTableRelationships = formatTableRelationships invoiceLinesTableRelationships,
      _tdInvoiceRelationshipName = invoiceRelationshipName,
      _tdTrackRelationshipName = trackRelationshipName,
      _tdMediaTypesTableName = formatTableName testConfig mediaTypesTableName,
      _tdMediaTypesRows = mediaTypesRows,
      _tdTracksTableName = formatTableName testConfig tracksTableName,
      _tdTracksRows = tracksRows,
      _tdTracksRowsById = tracksRowsById,
      _tdTracksTableRelationships = formatTableRelationships tracksTableRelationships,
      _tdInvoiceLinesRelationshipName = invoiceLinesRelationshipName,
      _tdMediaTypeRelationshipName = mediaTypeRelationshipName,
      _tdAlbumRelationshipName = albumRelationshipName,
      _tdGenreRelationshipName = genreRelationshipName,
      _tdPlaylistTracksRelationshipName = playlistTracksRelationshipName,
      _tdPlaylistTracksTableName = formatTableName testConfig playlistTracksTableName,
      _tdPlaylistTracksRows = playlistTracksRows,
      _tdGenresTableName = formatTableName testConfig genresTableName,
      _tdGenresRows = genresRows,
      _tdGenresTableRelationships = formatTableRelationships genresTableRelationships,
      _tdColumnName = formatColumnName testConfig . API.ColumnName,
      _tdColumnField = columnField schemaResponse testConfig,
      _tdMkDefaultTableInsertSchema = mkDefaultTableInsertSchema schemaResponse testConfig schemaTables,
      _tdColumnInsertSchema = columnInsertSchema schemaResponse testConfig,
      _tdRowColumnOperatorValue = rowColumnOperatorValue schemaResponse testConfig,
      _tdFindColumnScalarType = \tableName name -> findColumnScalarType schemaResponse tableName (formatColumnName testConfig $ API.ColumnName name),
      _tdQueryComparisonColumn = \name scalarType -> API.ComparisonColumn API.QueryTable (API.mkColumnSelector . formatColumnName testConfig $ API.ColumnName name) scalarType Nothing,
      _tdCurrentComparisonColumn = \name scalarType -> API.ComparisonColumn API.CurrentTable (API.mkColumnSelector . formatColumnName testConfig $ API.ColumnName name) scalarType Nothing,
      _tdOrderByColumn = \targetPath name -> orderByColumn targetPath (formatColumnName testConfig $ API.ColumnName name)
    }
  where
    formatTableRelationships :: API.TableRelationships -> API.TableRelationships
    formatTableRelationships =
      prefixTableRelationships
        >>> API.trelRelationships . traverse . API.rColumnMapping
          %~ ( API.unColumnPathMapping
                 >>> HashMap.toList
                 >>> fmap (bimap (formatColumnSelector testConfig) (formatColumnSelector testConfig))
                 >>> HashMap.fromList
                 >>> API.ColumnPathMapping
             )

    prefixTableRelationships :: API.TableRelationships -> API.TableRelationships
    prefixTableRelationships =
      API.trelSourceTable %~ formatTableName testConfig
        >>> API.trelRelationships . traverse . API.rTarget . API._TTable . API.ttName %~ (formatTableName testConfig)

-- | Test data from the TestingEdgeCases dataset template
data EdgeCasesTestData = EdgeCasesTestData
  { -- = NoPrimaryKey table
    _ectdNoPrimaryKeyTableName :: API.TableName,
    -- = DefaultedPrimaryKey table
    _ectdDefaultedPrimaryKeyTableName :: API.TableName,
    -- = AllColumnsDefaultable table
    _ectdAllColumnsDefaultableTableName :: API.TableName,
    -- = Scalar Types
    _ectdFindColumnScalarType :: API.TableName -> Text -> API.ScalarType,
    -- = Utility functions
    _ectdTableExists :: API.TableName -> Bool,
    _ectdColumnField :: API.TableName -> Text -> API.Field,
    _ectdMkDefaultTableInsertSchema :: API.TableName -> API.TableInsertSchema,
    _ectdRowColumnOperatorValue :: API.TableName -> Text -> J.Value -> API.RowColumnOperatorValue,
    _ectdCurrentComparisonColumn :: Text -> API.ScalarType -> API.ComparisonColumn
  }

mkEdgeCasesTestData :: TestConfig -> API.SchemaResponse -> EdgeCasesTestData
mkEdgeCasesTestData testConfig schemaResponse =
  EdgeCasesTestData
    { _ectdNoPrimaryKeyTableName = noPrimaryKeyTableName,
      _ectdDefaultedPrimaryKeyTableName = defaultedPrimaryKeyTableName,
      _ectdAllColumnsDefaultableTableName = allColumnsDefaultableTableName,
      _ectdFindColumnScalarType = \tableName name -> findColumnScalarType schemaResponse tableName (formatColumnName testConfig $ API.ColumnName name),
      _ectdTableExists = tableExists,
      _ectdColumnField = columnField schemaResponse testConfig,
      _ectdMkDefaultTableInsertSchema = mkDefaultTableInsertSchema schemaResponse testConfig edgeCasesSchemaTables,
      _ectdRowColumnOperatorValue = rowColumnOperatorValue schemaResponse testConfig,
      _ectdCurrentComparisonColumn = \name scalarType -> API.ComparisonColumn API.CurrentTable (API.mkColumnSelector . formatColumnName testConfig $ API.ColumnName name) scalarType Nothing
    }
  where
    tableExists :: API.TableName -> Bool
    tableExists tableName = tableName `elem` (API._tiName <$> API._srTables schemaResponse)

    noPrimaryKeyTableName = formatTableName testConfig (API.TableName $ "NoPrimaryKey" :| [])
    defaultedPrimaryKeyTableName = formatTableName testConfig (API.TableName $ "DefaultedPrimaryKey" :| [])
    allColumnsDefaultableTableName = formatTableName testConfig (API.TableName $ "AllColumnsDefaultable" :| [])

-- | Test data from the FunctionsTestData dataset template
data FunctionsTestData = FunctionsTestData
  { -- = Functions
    _ftdFibonacciField :: API.FunctionName -> Text -> API.Field, -- This is specialised to Fibonacci due to the defaulting requirements.
    _ftdFibonacciRows :: Int -> [HashMap API.FieldName API.FieldValue],
    _ftdFibonacciFunctionName :: API.FunctionName,
    _ftdSearchArticlesField :: API.FunctionName -> Text -> API.Field,
    _ftdSearchArticlesFunctionName :: API.FunctionName,
    _ftdAuthorRelationshipName :: API.RelationshipName,
    _ftdAuthorsTableName :: API.TableName,
    _ftdColumnField :: API.TableName -> Text -> API.Field
  }

mkFunctionsTestData :: API.SchemaResponse -> TestConfig -> FunctionsTestData
mkFunctionsTestData schemaResponse testConfig =
  FunctionsTestData
    { _ftdFibonacciField = functionField schemaResponse testConfig (API.singletonTableName "Result"),
      _ftdFibonacciRows = mkFibonacciRows,
      _ftdFibonacciFunctionName = formatFunctionName testConfig (API.FunctionName (NonEmpty.singleton "Fibonacci")),
      _ftdSearchArticlesField = functionField schemaResponse testConfig (API.singletonTableName "Articles"),
      _ftdSearchArticlesFunctionName = formatFunctionName testConfig (API.FunctionName (NonEmpty.singleton "SearchArticles")),
      _ftdAuthorRelationshipName = API.RelationshipName "author",
      _ftdAuthorsTableName = API.singletonTableName "Authors",
      _ftdColumnField = columnField schemaResponse testConfig
    }

formatTableName :: TestConfig -> API.TableName -> API.TableName
formatTableName TestConfig {..} = applyTableNamePrefix _tcTableNamePrefix . API.TableName . fmap (applyNameCasing _tcTableNameCasing) . API.unTableName

formatFunctionName :: TestConfig -> API.FunctionName -> API.FunctionName
formatFunctionName TestConfig {..} = applyFunctionNamePrefix _tcFunctionNamePrefix . API.FunctionName . fmap (applyNameCasing _tcFunctionNameCasing) . API.unFunctionName

formatTableInfo :: TestConfig -> API.TableInfo -> API.TableInfo
formatTableInfo testConfig =
  API.tiName %~ formatTableName testConfig
    >>> API.tiColumns . traverse . API.ciName %~ formatColumnName testConfig
    >>> API.tiPrimaryKey . _Just . traverse %~ formatColumnName testConfig
    >>> API.tiForeignKeys . API.unForeignKeys . traverse
      %~ ( API.cForeignTable %~ formatTableName testConfig
             >>> API.cColumnMapping %~ (API.unColumnPathMapping >>> HashMap.toList >>> fmap (bimap (formatColumnSelector testConfig) (formatColumnSelector testConfig)) >>> HashMap.fromList >>> API.ColumnPathMapping)
         )

applyTableNamePrefix :: [Text] -> API.TableName -> API.TableName
applyTableNamePrefix prefix tableName@(API.TableName rawTableName) =
  case NonEmpty.nonEmpty prefix of
    Just prefix' -> API.TableName (prefix' <> rawTableName)
    Nothing -> tableName

applyFunctionNamePrefix :: [Text] -> API.FunctionName -> API.FunctionName
applyFunctionNamePrefix prefix functionName@(API.FunctionName rawFunctionName) =
  case NonEmpty.nonEmpty prefix of
    Just prefix' -> API.FunctionName (prefix' <> rawFunctionName)
    Nothing -> functionName

applyNameCasing :: NameCasing -> Text -> Text
applyNameCasing casing text = case casing of
  PascalCase -> text
  Lowercase -> Text.toLower text
  Uppercase -> Text.toUpper text

formatColumnName :: TestConfig -> API.ColumnName -> API.ColumnName
formatColumnName TestConfig {..} = API.ColumnName . applyNameCasing _tcColumnNameCasing . API.unColumnName

formatColumnSelector :: TestConfig -> API.ColumnSelector -> API.ColumnSelector
formatColumnSelector testConfig = \case
  API.ColumnSelectorPath p -> API.ColumnSelectorPath $ formatColumnName testConfig <$> p
  API.ColumnSelectorColumn c -> API.ColumnSelectorColumn $ formatColumnName testConfig c

columnField :: API.SchemaResponse -> TestConfig -> API.TableName -> Text -> API.Field
columnField schemaResponse testConfig tableName columnName =
  API.ColumnField columnName' scalarType Nothing
  where
    columnName' = formatColumnName testConfig $ API.ColumnName columnName
    scalarType = findColumnScalarType schemaResponse tableName columnName'

functionField :: API.SchemaResponse -> TestConfig -> API.TableName -> API.FunctionName -> Text -> API.Field
functionField schemaResponse@API.SchemaResponse {..} testConfig defaultTableName functionName columnName =
  columnField schemaResponse testConfig tableName columnName
  where
    tableName = fromMaybe defaultTableName (functionReturnType ^? _Just . API._FunctionReturnsTable)
    functionReturnType = maybe (error $ "Can't find the function " <> show functionName <> " in " <> show (API._fiName <$> _srFunctions)) API._fiReturns functionInfo
    functionInfo = find (\API.FunctionInfo {..} -> _fiName == functionName) _srFunctions

mkDefaultTableInsertSchema :: API.SchemaResponse -> TestConfig -> [API.TableInfo] -> API.TableName -> API.TableInsertSchema
mkDefaultTableInsertSchema schemaResponse testConfig expectedSchemaTables tableName =
  API.TableInsertSchema
    { _tisTable = tableName,
      _tisPrimaryKey = fmap (formatColumnName testConfig) <$> API._tiPrimaryKey tableInfo,
      _tisFields = mkFieldsMap insertFields
    }
  where
    tableInfo =
      expectedSchemaTables
        & find (\API.TableInfo {..} -> formatTableName testConfig _tiName == tableName)
        & fromMaybe (error $ "Can't find table " <> show tableName <> " in schema")
    columnNames =
      tableInfo
        & API._tiColumns
        & fmap API._ciName
    insertFields =
      columnNames
        & fmap
          ( \columnName ->
              let formattedColumnName = formatColumnName testConfig columnName
                  API.ColumnInfo {..} = findColumnInfo schemaResponse tableName formattedColumnName
                  columnNameText = API.unColumnName columnName
               in (columnNameText, API.ColumnInsert $ API.ColumnInsertSchema _ciName _ciType _ciNullable _ciValueGenerated)
          )

columnInsertSchema :: API.SchemaResponse -> TestConfig -> API.TableName -> Text -> API.ColumnInsertSchema
columnInsertSchema schemaResponse testConfig tableName columnName =
  API.ColumnInsertSchema columnName' (API._ciType columnInfo) (API._ciNullable columnInfo) (API._ciValueGenerated columnInfo)
  where
    columnName' = formatColumnName testConfig $ API.ColumnName columnName
    columnInfo = findColumnInfo schemaResponse tableName columnName'

rowColumnOperatorValue :: API.SchemaResponse -> TestConfig -> API.TableName -> Text -> J.Value -> API.RowColumnOperatorValue
rowColumnOperatorValue schemaResponse testConfig tableName columnName value =
  API.RowColumnOperatorValue columnName' value scalarType
  where
    columnName' = formatColumnName testConfig $ API.ColumnName columnName
    scalarType = findColumnScalarType schemaResponse tableName columnName'

findColumnInfo :: API.SchemaResponse -> API.TableName -> API.ColumnName -> API.ColumnInfo
findColumnInfo API.SchemaResponse {..} tableName columnName =
  fromMaybe (error $ "Can't find the scalar type of column " <> show columnName <> " in table " <> show tableName) columnInfo
  where
    tableInfo = find (\API.TableInfo {..} -> _tiName == tableName) _srTables
    columnInfo = find (\API.ColumnInfo {..} -> _ciName == columnName) =<< API._tiColumns <$> tableInfo

findColumnScalarType :: API.SchemaResponse -> API.TableName -> API.ColumnName -> API.ScalarType
findColumnScalarType schemaResponse tableName columnName =
  case API._ciType $ findColumnInfo schemaResponse tableName columnName of
    API.ColumnTypeScalar scalarType -> scalarType
    _ -> error $ "Column " <> show columnName <> " in table " <> show tableName <> " does not have a scalar type"

emptyQuery :: API.Query
emptyQuery = API.Query Nothing Nothing Nothing Nothing Nothing Nothing Nothing

emptyMutationRequest :: API.MutationRequest
emptyMutationRequest = API.MutationRequest mempty mempty mempty mempty

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
  tableRels & API.trelRelationships %~ HashMap.filterWithKey (\relName _ -> relName `elem` names)

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

fieldAt :: (At m, Index m ~ API.FieldName) => Text -> Traversal' m (Maybe (IxValue m))
fieldAt fieldName = at (API.FieldName fieldName)

_ColumnFieldNumber :: Traversal' API.FieldValue Scientific
_ColumnFieldNumber = API._ColumnFieldValue . _Number

_ColumnFieldString :: Traversal' API.FieldValue Text
_ColumnFieldString = API._ColumnFieldValue . _String

_ColumnFieldBoolean :: Traversal' API.FieldValue Bool
_ColumnFieldBoolean = API._ColumnFieldValue . _Bool

_ColumnFieldNull :: Traversal' API.FieldValue ()
_ColumnFieldNull = API._ColumnFieldValue . _Null

_RelationshipFieldRows :: Traversal' API.FieldValue [HashMap API.FieldName API.FieldValue]
_RelationshipFieldRows = API._RelationshipFieldValue . API.qrRows . _Just

scalarValueComparison :: J.Value -> API.ScalarType -> API.ComparisonValue
scalarValueComparison value valueType = API.ScalarValueComparison $ API.ScalarValue value valueType

orderByColumn :: [API.RelationshipName] -> API.ColumnName -> API.OrderDirection -> API.OrderByElement
orderByColumn targetPath columnName orderDirection =
  API.OrderByElement targetPath (API.OrderByColumn (API.mkColumnSelector columnName) Nothing) orderDirection

insertAutoIncPk :: Text -> Integer -> [HashMap API.FieldName API.FieldValue] -> [HashMap API.FieldName API.FieldValue]
insertAutoIncPk pkFieldName startingPkId rows =
  zip [startingPkId ..] rows
    & fmap
      ( \(albumId, albumRow) ->
          albumRow & at (API.FieldName pkFieldName) ?~ API.mkColumnFieldValue (J.Number $ fromInteger albumId)
      )

autoIncPks :: Integer -> [a] -> [Integer]
autoIncPks startingPkId rows = fst <$> zip [startingPkId ..] rows

mkSubqueryFieldValue :: Maybe [HashMap API.FieldName API.FieldValue] -> Maybe (HashMap API.FieldName J.Value) -> API.FieldValue
mkSubqueryFieldValue rows aggregates =
  API.mkRelationshipFieldValue $ API.QueryResponse rows aggregates

mkSubqueryRowsFieldValue :: [HashMap API.FieldName API.FieldValue] -> API.FieldValue
mkSubqueryRowsFieldValue rows =
  API.mkRelationshipFieldValue $ API.QueryResponse (Just rows) Nothing

mkSubqueryAggregatesFieldValue :: HashMap API.FieldName J.Value -> API.FieldValue
mkSubqueryAggregatesFieldValue aggregates =
  API.mkRelationshipFieldValue $ API.QueryResponse Nothing (Just aggregates)

mkAndExpr :: (Foldable f) => f API.Expression -> API.Expression
mkAndExpr = API.And . Set.fromList . Foldable.toList

mkOrExpr :: (Foldable f) => f API.Expression -> API.Expression
mkOrExpr = API.Or . Set.fromList . Foldable.toList
