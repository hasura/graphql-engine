{-# LANGUAGE DuplicateRecordFields #-}

module Hasura.Backends.BigQuery.DDL.Source
  ( resolveSource
  , postDropSourceHook
  , resolveSourceConfig
  )
where

import           Hasura.Prelude

import qualified Data.Aeson                          as J
import qualified Data.ByteString.Lazy                as L
import qualified Data.Environment                    as Env
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T

import           Control.Concurrent.MVar             (newMVar)
import           Data.Time.Clock.System

import           Hasura.Backends.BigQuery.Connection
import           Hasura.Backends.BigQuery.Meta
import           Hasura.Backends.BigQuery.Source
import           Hasura.Backends.BigQuery.Types
import           Hasura.Base.Error
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Source
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


defaultGlobalSelectLimit :: Int
defaultGlobalSelectLimit = 1000


resolveSourceConfig ::
  MonadIO m =>
  SourceName ->
  BigQueryConnSourceConfig ->
  m (Either QErr BigQuerySourceConfig)
resolveSourceConfig _name BigQueryConnSourceConfig{..} = runExceptT $ do
  env <- liftIO Env.getEnvironment
  eSA <- resolveConfigurationJson env _cscServiceAccount
  case eSA of
    Left e -> throw400 Unexpected $ T.pack e
    Right _scServiceAccount -> do
      _scDatasets <- resolveConfigurationInputs env _cscDatasets
      _scProjectId <- resolveConfigurationInput env _cscProjectId
      _scGlobalSelectLimit <- resolveConfigurationInput env `mapM` _cscGlobalSelectLimit >>= \case
         Nothing -> pure defaultGlobalSelectLimit
         Just i ->
           -- This works around the inconsistency between JSON and
           -- environment variables. The config handling module should be
           -- reworked to handle non-text values better.
           case readMaybe (T.unpack i) <|> J.decode (L.fromStrict (T.encodeUtf8 i)) of
             Nothing -> throw400 Unexpected $ "Need a non-negative integer for global select limit"
             Just i' -> do
               when (i' < 0) $ throw400 Unexpected "Need the integer for the global select limit to be non-negative"
               pure i'
      trMVar <- liftIO $ newMVar Nothing -- `runBigQuery` initializes the token
      pure BigQuerySourceConfig
             { _scAccessTokenMVar = trMVar
             , ..
             }


resolveSource
  :: (MonadIO m)
  => BigQuerySourceConfig
  -> m (Either QErr (ResolvedSource 'BigQuery))
resolveSource sourceConfig =
  runExceptT $ do
    result <- getTables sourceConfig
    case result of
      Left err ->
        throw400 Unexpected $
        "unexpected exception while connecting to database: " <> tshow err
      Right restTables -> do
        seconds <- liftIO $ fmap systemSeconds getSystemTime
        pure
          (ResolvedSource
             { _rsConfig = sourceConfig
             , _rsTables =
                 HM.fromList
                   [ ( restTableReferenceToTableName tableReference
                     , DBTableMetadata
                         { _ptmiOid = OID (fromIntegral seconds + index :: Int) -- TODO: The seconds are used for uniqueness. BigQuery doesn't support a "stable" ID for a table.
                         , _ptmiColumns =
                             [ RawColumnInfo
                               { prciName = ColumnName name
                               , prciPosition = position
                               , prciType = restTypeToScalarType type'
                               , prciIsNullable =
                                   case mode of
                                     Nullable -> True
                                     _        -> False
                               , prciDescription = Nothing
                               }
                             | (position, RestFieldSchema {name, type', mode}) <-
                                 zip [1 ..] fields -- TODO: Same trouble as Oid above.
                             ]
                         , _ptmiPrimaryKey = Nothing
                         , _ptmiUniqueConstraints = mempty
                         , _ptmiForeignKeys = mempty
                         , _ptmiViewInfo = Just $ ViewInfo False False False
                         , _ptmiDescription = Nothing
                         , _ptmiExtraTableMetadata = ()
                         })
                   | (index, RestTable {tableReference, schema}) <-
                       zip [0 ..] restTables
                   , let RestTableSchema fields = schema
                   ]
             , _rsFunctions = mempty
             , _rsPgScalars = mempty
             })

restTypeToScalarType :: RestType -> ScalarType
restTypeToScalarType =
  \case
    STRING     -> StringScalarType
    BYTES      -> BytesScalarType
    INTEGER    -> IntegerScalarType
    FLOAT      -> FloatScalarType
    BOOL       -> BoolScalarType
    TIMESTAMP  -> TimestampScalarType
    DATE       -> DateScalarType
    TIME       -> TimeScalarType
    DATETIME   -> DatetimeScalarType
    GEOGRAPHY  -> GeographyScalarType
    STRUCT     -> StructScalarType
    BIGDECIMAL -> BigDecimalScalarType
    DECIMAL    -> DecimalScalarType

-- Hierarchy: Project / Dataset / Table
-- see <https://cloud.google.com/bigquery/docs/datasets-intro>
restTableReferenceToTableName :: RestTableReference -> TableName
restTableReferenceToTableName RestTableReference {..} =
  TableName {tableName = tableId, tableNameSchema = datasetId}
  -- We ignore project id and push that requirement up to the top to
  -- the data source level.

postDropSourceHook
  :: (MonadIO m)
  => BigQuerySourceConfig -> m ()
postDropSourceHook _ =
  -- On BigQuery we don't keep connections open.
  pure ()
