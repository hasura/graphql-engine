module Test.DataExport
  ( exportData,
  )
where

import Command (ExportDataConfig (..), ExportFormat (..))
import Control.Lens ((&))
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (ZonedTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Hasura.Backends.DataConnector.API
import System.Directory qualified as Directory
import System.FilePath ((<.>), (</>))
import Test.Data qualified as Data

exportData :: ExportDataConfig -> IO ()
exportData ExportDataConfig {..} = do
  absDirectory <- Directory.makeAbsolute _edcDirectory
  Directory.createDirectoryIfMissing True absDirectory
  putStrLn $ "Exporting to " <> absDirectory
  let convertTable' = case _edcFormat of
        JSON -> convertTable ".json" convertTableToJSON
        JSONLines -> convertTable ".json" convertTableToJSONLines
  Data.schemaTables
    & fmap (\tableInfo@TableInfo {..} -> (tableInfo,) <$> fromMaybe [] $ HashMap.lookup _tiName Data.allTableRows)
    & mapM_ \(tableInfo@TableInfo {..}, rows) -> do
      let rows' = maybe rows (\formatString -> formatDateColumnsInRow formatString tableInfo <$> rows) _edcDateTimeFormat
      let (filename, contents) = convertTable' _tiName rows'
      let destFile = absDirectory </> filename
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
  J.encode $ J.toJSON rows

convertTableToJSONLines :: [HashMap FieldName FieldValue] -> BSL.ByteString
convertTableToJSONLines rows =
  BSL.intercalate "\n" $ J.encode . J.toJSON <$> rows

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
    dateFields = fmap (\ColumnInfo {..} -> FieldName $ unColumnName _ciName) $ filter (\ColumnInfo {..} -> _ciType == dateTimeScalarType) _tiColumns
    dateTimeScalarType = CustomTy "DateTime"
    tryFormatDate fieldValue = case deserializeAsColumnFieldValue fieldValue of
      J.String value -> do
        (zonedTime :: ZonedTime) <- iso8601ParseM $ Text.unpack value
        let formattedString = formatTime defaultTimeLocale dateTimeFormatString zonedTime
        Just . mkColumnFieldValue . J.String . Text.pack $ formattedString
      _ -> Nothing
