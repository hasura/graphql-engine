module Hasura.Backends.BigQuery.DDL
  ( fetchAndValidateEnumValues,
    buildFunctionInfo,
    updateColumnInEventTrigger,
    parseBoolExpOperations,
    parseCollectableType,
    scalarTypeFromColumnType,
    module M,
  )
where

import Data.Aeson
import Hasura.Backends.BigQuery.DDL.BoolExp
import Hasura.Backends.BigQuery.DDL.ComputedField as M
import Hasura.Backends.BigQuery.DDL.Source as M
import Hasura.Backends.BigQuery.Types qualified as BigQuery
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types
import Hasura.Server.Utils
import Hasura.Session
import Hasura.Table.Cache

fetchAndValidateEnumValues ::
  (Monad m) =>
  SourceConfig 'BigQuery ->
  TableName 'BigQuery ->
  Maybe (PrimaryKey 'BigQuery (RawColumnInfo 'BigQuery)) ->
  [RawColumnInfo 'BigQuery] ->
  m (Either QErr EnumValues)
fetchAndValidateEnumValues _ _ _ _ =
  runExceptT
    $ throw400 NotSupported "Enum tables are not supported for BigQuery sources"

buildFunctionInfo ::
  (MonadError QErr m) =>
  SourceName ->
  FunctionName 'BigQuery ->
  SystemDefined ->
  FunctionConfig 'BigQuery ->
  FunctionPermissionsMap ->
  RawFunctionInfo 'BigQuery ->
  Maybe Text ->
  NamingCase ->
  m (FunctionInfo 'BigQuery, SchemaDependency)
buildFunctionInfo _ _ _ _ _ _ _ _ =
  throw400 NotSupported "SQL Functions are not supported for BigQuery source"

updateColumnInEventTrigger ::
  TableName 'BigQuery ->
  Column 'BigQuery ->
  Column 'BigQuery ->
  TableName 'BigQuery ->
  EventTriggerConf 'BigQuery ->
  EventTriggerConf 'BigQuery
updateColumnInEventTrigger _ _ _ _ = id

parseCollectableType ::
  (MonadError QErr m) =>
  CollectableType (ColumnType 'BigQuery) ->
  Value ->
  m (PartialSQLExp 'BigQuery)
parseCollectableType collectableType = \case
  String t
    | isSessionVariable t -> pure $ mkTypedSessionVar collectableType $ mkSessionVariable t
    | isReqUserId t -> pure $ mkTypedSessionVar collectableType userIdHeader
  val -> case collectableType of
    CollectableTypeScalar scalarType ->
      PSESQLExp . BigQuery.ValueExpression . BigQuery.TypedValue (scalarTypeFromColumnType scalarType) <$> parseScalarValueColumnTypeWithContext () scalarType val
    CollectableTypeArray _ ->
      throw400 NotSupported "Array types are not supported in BigQuery backend"

scalarTypeFromColumnType :: ColumnType 'BigQuery -> BigQuery.ScalarType
scalarTypeFromColumnType (ColumnEnumReference _) = BigQuery.StringScalarType
scalarTypeFromColumnType (ColumnScalar scalar) = scalar

mkTypedSessionVar ::
  CollectableType (ColumnType 'BigQuery) ->
  SessionVariable ->
  PartialSQLExp 'BigQuery
mkTypedSessionVar columnType =
  PSESessVar (msColumnTypeToScalarType <$> columnType)

msColumnTypeToScalarType :: ColumnType 'BigQuery -> ScalarType 'BigQuery
msColumnTypeToScalarType = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _ -> BigQuery.StringScalarType
