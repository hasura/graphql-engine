{-# LANGUAGE DuplicateRecordFields #-}

module Hasura.Backends.BigQuery.DDL.Source
  ( resolveSource,
    postDropSourceHook,
    resolveSourceConfig,
    restTypeToScalarType,
  )
where

import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as L
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.Int qualified as Int
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock.System
import Hasura.Backends.BigQuery.Connection
import Hasura.Backends.BigQuery.Meta
import Hasura.Backends.BigQuery.Source
import Hasura.Backends.BigQuery.Types
import Hasura.Base.Error
import Hasura.Function.Cache (FunctionOverloads (..))
import Hasura.Prelude
import Hasura.RQL.Types.Backend (BackendConfig)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Source
import Hasura.Table.Cache

defaultGlobalSelectLimit :: Int.Int64
defaultGlobalSelectLimit = 1000

defaultRetryLimit :: Int
defaultRetryLimit = 5

defaultRetryBaseDelay :: Microseconds
defaultRetryBaseDelay = 500000

resolveSourceConfig ::
  (MonadIO m) =>
  SourceName ->
  BigQueryConnSourceConfig ->
  BackendSourceKind 'BigQuery ->
  BackendConfig 'BigQuery ->
  Env.Environment ->
  manager ->
  m (Either QErr BigQuerySourceConfig)
resolveSourceConfig _name BigQueryConnSourceConfig {..} _backendKind _backendConfig env _manager = runExceptT $ do
  eSA <- resolveConfigurationJson env _cscServiceAccount
  case eSA of
    Left e -> throw400 Unexpected $ T.pack e
    Right serviceAccount -> do
      projectId <- BigQueryProjectId <$> resolveConfigurationInput env _cscProjectId
      retryOptions <- do
        numRetries <-
          resolveConfigurationInput env `mapM` _cscRetryLimit >>= \case
            Nothing -> pure defaultRetryLimit
            Just v -> readNonNegative v "retry limit"
        if numRetries == 0
          then pure Nothing
          else do
            let _retryNumRetries = numRetries
            _retryBaseDelay <-
              resolveConfigurationInput env `mapM` _cscRetryBaseDelay >>= \case
                Nothing -> pure defaultRetryBaseDelay
                Just v -> fromInteger <$> readNonNegative v "retry base delay"
            pure $ Just RetryOptions {..}
      _scConnection <- initConnection serviceAccount projectId retryOptions
      _scDatasets <- fmap BigQueryDataset <$> resolveConfigurationInputs env _cscDatasets
      _scGlobalSelectLimit <-
        resolveConfigurationInput env `mapM` _cscGlobalSelectLimit >>= \case
          Nothing -> pure defaultGlobalSelectLimit
          Just v -> readNonNegative v "global select limit"
      pure BigQuerySourceConfig {..}

readNonNegative :: (MonadError QErr m, Num a, Ord a, J.FromJSON a, Read a) => Text -> Text -> m a
readNonNegative i paramName =
  -- This works around the inconsistency between JSON and
  -- environment variables. The config handling module should be
  -- reworked to handle non-text values better.
  case readMaybe (T.unpack i) <|> J.decode (L.fromStrict (T.encodeUtf8 i)) of
    Nothing -> throw400 Unexpected $ "Need a non-negative integer for " <> paramName
    Just i' -> do
      when (i' < 0) $ throw400 Unexpected $ "Need the integer for the " <> paramName <> " to be non-negative"
      pure i'

resolveSource ::
  (MonadIO m) =>
  BigQuerySourceConfig ->
  m (Either QErr (DBObjectsIntrospection 'BigQuery))
resolveSource sourceConfig =
  runExceptT $ do
    tables <- getTables sourceConfig
    routines <- getRoutines sourceConfig
    let result = (,) <$> tables <*> routines
    case result of
      Left err ->
        throw400 Unexpected
          $ "unexpected exception while connecting to database: "
          <> tshow err
      Right (restTables, restRoutines) -> do
        seconds <- liftIO $ fmap systemSeconds getSystemTime
        let functions = FunctionOverloads <$> HashMap.groupOnNE (routineReferenceToFunctionName . routineReference) restRoutines
        pure
          ( DBObjectsIntrospection
              { _rsTables =
                  HashMap.fromList
                    [ ( restTableReferenceToTableName tableReference,
                        DBTableMetadata
                          { _ptmiOid = OID (fromIntegral seconds + index :: Int), -- TODO: The seconds are used for uniqueness. BigQuery doesn't support a "stable" ID for a table.
                            _ptmiColumns =
                              [ RawColumnInfo
                                  { rciName = ColumnName name,
                                    rciPosition = position,
                                    rciType = RawColumnTypeScalar $ restTypeToScalarType type',
                                    rciIsNullable =
                                      case mode of
                                        Nullable -> True
                                        _ -> False,
                                    rciDescription = Nothing,
                                    rciMutability = ColumnMutability {_cmIsInsertable = True, _cmIsUpdatable = True}
                                  }
                                | (position, RestFieldSchema {name, type', mode}) <-
                                    zip [1 ..] fields -- TODO: Same trouble as Oid above.
                              ],
                            _ptmiPrimaryKey = Nothing,
                            _ptmiUniqueConstraints = mempty,
                            _ptmiForeignKeys = mempty,
                            _ptmiViewInfo = Just $ ViewInfo False False False,
                            _ptmiDescription = Nothing,
                            _ptmiExtraTableMetadata = ()
                          }
                      )
                      | (index, RestTable {tableReference, schema}) <-
                          zip [0 ..] restTables,
                        let RestTableSchema fields = schema
                    ],
                _rsFunctions = functions,
                _rsScalars = mempty,
                _rsLogicalModels = mempty
              }
          )

restTypeToScalarType :: RestType -> ScalarType
restTypeToScalarType =
  \case
    STRING -> StringScalarType
    BYTES -> BytesScalarType
    INTEGER -> IntegerScalarType
    FLOAT -> FloatScalarType
    BOOL -> BoolScalarType
    TIMESTAMP -> TimestampScalarType
    DATE -> DateScalarType
    TIME -> TimeScalarType
    DATETIME -> DatetimeScalarType
    GEOGRAPHY -> GeographyScalarType
    STRUCT -> StructScalarType
    BIGDECIMAL -> BigDecimalScalarType
    DECIMAL -> DecimalScalarType
    JSON -> JsonScalarType

-- Hierarchy: Project / Dataset / Table
-- see <https://cloud.google.com/bigquery/docs/datasets-intro>
restTableReferenceToTableName :: RestTableReference -> TableName
restTableReferenceToTableName RestTableReference {..} =
  TableName {tableName = tableId, tableNameSchema = datasetId}

-- We ignore project id and push that requirement up to the top to
-- the data source level.

postDropSourceHook ::
  (MonadIO m) =>
  BigQuerySourceConfig ->
  TableEventTriggers 'BigQuery ->
  m ()
postDropSourceHook _ _ =
  -- On BigQuery we don't keep connections open.
  pure ()
