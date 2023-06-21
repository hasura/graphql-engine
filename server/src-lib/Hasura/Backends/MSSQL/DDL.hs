-- | MSSQL DDL
--
-- Implements the DDL related methods of the
-- 'Hasura.RQL.Types.Metadata.Backend.BackendMetadata' type class
-- for the MSSQL backend, which provides an interface for fetching information about
-- the objects in the database, such as tables, relationships, etc.
--
-- The actual instance is defined in "Hasura.Backends.MSSQL.Instances.Metadata".
module Hasura.Backends.MSSQL.DDL
  ( buildComputedFieldInfo,
    fetchAndValidateEnumValues,
    buildFunctionInfo,
    updateColumnInEventTrigger,
    parseCollectableType,
    getStoredProcedureGraphqlName,
    module M,
  )
where

import Data.Aeson
import Hasura.Backends.MSSQL.DDL.BoolExp as M
import Hasura.Backends.MSSQL.DDL.Source as M
import Hasura.Backends.MSSQL.Types.Internal qualified as MT
import Hasura.Base.Error
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.SchemaCache
import Hasura.SQL.Types
import Hasura.Server.Utils
import Hasura.Session
import Hasura.StoredProcedure.Types
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax qualified as G

buildComputedFieldInfo ::
  (MonadError QErr m) =>
  HashSet (TableName 'MSSQL) ->
  TableName 'MSSQL ->
  HashSet (Column 'MSSQL) ->
  ComputedFieldName ->
  ComputedFieldDefinition 'MSSQL ->
  RawFunctionInfo 'MSSQL ->
  Comment ->
  m (ComputedFieldInfo 'MSSQL)
buildComputedFieldInfo _ _ _ _ _ _ _ =
  throw400 NotSupported "Computed fields aren't supported for MSSQL sources"

fetchAndValidateEnumValues ::
  (Monad m) =>
  SourceConfig 'MSSQL ->
  TableName 'MSSQL ->
  Maybe (PrimaryKey 'MSSQL (RawColumnInfo 'MSSQL)) ->
  [RawColumnInfo 'MSSQL] ->
  m (Either QErr EnumValues)
fetchAndValidateEnumValues _ _ _ _ =
  runExceptT
    $ throw400 NotSupported "Enum tables are not supported for MSSQL sources"

buildFunctionInfo ::
  (MonadError QErr m) =>
  SourceName ->
  FunctionName 'MSSQL ->
  SystemDefined ->
  FunctionConfig 'MSSQL ->
  FunctionPermissionsMap ->
  RawFunctionInfo 'MSSQL ->
  Maybe Text ->
  NamingCase ->
  m (FunctionInfo 'MSSQL, SchemaDependency)
buildFunctionInfo _ _ _ _ _ _ _ _ =
  throw400 NotSupported "SQL Functions are not supported for MSSQL source"

updateColumnInEventTrigger ::
  TableName 'MSSQL ->
  Column 'MSSQL ->
  Column 'MSSQL ->
  TableName 'MSSQL ->
  EventTriggerConf 'MSSQL ->
  EventTriggerConf 'MSSQL
updateColumnInEventTrigger _ _ _ _ = id

parseCollectableType ::
  (MonadError QErr m) =>
  CollectableType (ColumnType 'MSSQL) ->
  Value ->
  m (PartialSQLExp 'MSSQL)
parseCollectableType collectableType = \case
  String t
    | isSessionVariable t -> pure $ mkTypedSessionVar collectableType $ mkSessionVariable t
    | isReqUserId t -> pure $ mkTypedSessionVar collectableType userIdHeader
  val -> case collectableType of
    CollectableTypeScalar scalarType ->
      PSESQLExp . MT.ValueExpression <$> parseScalarValueColumnTypeWithContext () scalarType val
    CollectableTypeArray _ ->
      throw400 NotSupported "Array types are not supported in MSSQL backend"

mkTypedSessionVar ::
  CollectableType (ColumnType 'MSSQL) ->
  SessionVariable ->
  PartialSQLExp 'MSSQL
mkTypedSessionVar columnType =
  PSESessVar (msColumnTypeToScalarType <$> columnType)

msColumnTypeToScalarType :: ColumnType 'MSSQL -> ScalarType 'MSSQL
msColumnTypeToScalarType = \case
  ColumnScalar scalarType -> scalarType
  ColumnEnumReference _ -> MT.TextType

getStoredProcedureGraphqlName ::
  (MonadError QErr m) =>
  MT.FunctionName ->
  StoredProcedureConfig ->
  m G.Name
getStoredProcedureGraphqlName spname =
  maybe (liftEither $ MT.getGQLFunctionName spname) pure . _spcCustomName
