module Test.DataExport
  ( exportData,
  )
where

import Command (ExportDataConfig (..), ExportFormat (..))
import Control.Lens ((&))
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as JE
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (ZonedTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Hasura.Backends.DataConnector.API
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import Test.Data qualified as Data
import Prelude

exportData :: ExportDataConfig -> IO ()
exportData exportDataConfig@ExportDataConfig {..} = do
  absDirectory <- Directory.makeAbsolute _edcDirectory
  Directory.createDirectoryIfMissing True absDirectory
  putStrLn $ "Exporting to " <> absDirectory
  case _edcFormat of
    JSON -> exportTablePerFile exportDataConfig absDirectory (convertTable ".json" convertTableToJSON)
    JSONLines -> exportTablePerFile exportDataConfig absDirectory (convertTable ".json" convertTableToJSONLines)
    SingleJSONFile -> exportSingleJSONFile exportDataConfig absDirectory

exportTablePerFile :: ExportDataConfig -> FilePath -> (TableName -> [HashMap FieldName FieldValue] -> (FilePath, BSL.ByteString)) -> IO ()
exportTablePerFile ExportDataConfig {..} directory convertTable' = do
  Data.schemaTables
    & fmap (\tableInfo@TableInfo {..} -> (tableInfo,) <$> fromMaybe [] $ HashMap.lookup _tiName Data.allTableRows)
    & mapM_ \(tableInfo@TableInfo {..}, rows) -> do
      let rows' = maybe rows (\formatString -> formatDateColumnsInRow formatString tableInfo <$> rows) _edcDateTimeFormat
      let (filename, contents) = convertTable' _tiName rows'
      let destFile = directory </> filename
      BSL.writeFile destFile contents
      putStrLn $ "Exported " <> filename

convertTable :: String -> ([HashMap FieldName FieldValue] -> BSL.ByteString) -> TableName -> [HashMap FieldName FieldValue] -> (FilePath, BSL.ByteString)
convertTable extension convertRows tableName rows =
  (filename, fileContents)
  where
    filename = (Text.unpack . NonEmpty.last $ unTableName tableName) <.> extension
    fileContents = convertRows rows

convertTableToJSON :: [HashMap FieldName FieldValue] -> BSL.ByteString
convertTableToJSON rows =
  J.encode $ rows

convertTableToJSONLines :: [HashMap FieldName FieldValue] -> BSL.ByteString
convertTableToJSONLines rows =
  BSL.intercalate "\n" $ J.encode <$> rows

formatDateColumnsInRow :: String -> TableInfo -> HashMap FieldName FieldValue -> HashMap FieldName FieldValue
formatDateColumnsInRow dateTimeFormatString TableInfo {..} row =
  row
    & HashMap.mapWithKey
      ( \fieldName fieldValue ->
          if fieldName `elem` dateFields
            then fromMaybe fieldValue $ tryFormatDate fieldValue
            else fieldValue
      )
  where
    dateFields = fmap (\ColumnInfo {..} -> FieldName $ unColumnName _ciName) $ filter (\ColumnInfo {..} -> _ciType == ColumnTypeScalar dateTimeScalarType) _tiColumns
    dateTimeScalarType = ScalarType "DateTime"
    tryFormatDate fieldValue = case deserializeAsColumnFieldValue fieldValue of
      J.String value -> do
        (zonedTime :: ZonedTime) <- iso8601ParseM $ Text.unpack value
        let formattedString = formatTime defaultTimeLocale dateTimeFormatString zonedTime
        Just . mkColumnFieldValue . J.String . Text.pack $ formattedString
      _ -> Nothing

exportSingleJSONFile :: ExportDataConfig -> FilePath -> IO ()
exportSingleJSONFile exportDataConfig directory = do
  let filename = "Chinook.json"
  let destFile = directory </> filename
  BSL.writeFile destFile $ encodeSingleJSONFile exportDataConfig
  putStrLn $ "Exported " <> filename

encodeSingleJSONFile :: ExportDataConfig -> BSL.ByteString
encodeSingleJSONFile ExportDataConfig {..} =
  JE.encodingToLazyByteString $
    JE.list
      ( \tableInfo@TableInfo {..} ->
          let rows = fromMaybe [] $ HashMap.lookup _tiName Data.allTableRows
              rows' = maybe rows (\formatString -> formatDateColumnsInRow formatString tableInfo <$> rows) _edcDateTimeFormat
           in encodeSingleJSONFileTable tableInfo rows'
      )
      Data.schemaTables

encodeSingleJSONFileTable :: TableInfo -> [HashMap FieldName FieldValue] -> J.Encoding
encodeSingleJSONFileTable TableInfo {..} rows =
  encodeAssocListAsObject
    [ ("tableName", J.toEncoding . NonEmpty.last $ unTableName _tiName),
      ("columns", encodeAssocListAsObject columns),
      ("rows", J.toEncoding rows)
    ]
  where
    columns :: [(Text, J.Encoding)]
    columns = (\ColumnInfo {..} -> (unColumnName _ciName, J.toEncoding _ciType)) <$> _tiColumns

encodeAssocListAsObject :: [(Text, J.Encoding)] -> J.Encoding
encodeAssocListAsObject =
  JE.dict
    JE.text
    id
    (\fn -> foldr (uncurry fn))
