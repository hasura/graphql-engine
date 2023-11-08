{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Postgres Instances Metadata
--
-- Defines a 'Hasura.RQL.Types.Metadata.Backend.BackendMetadata' type class instance for Postgres.
module Hasura.Backends.Postgres.Instances.Metadata () where

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.NonEmpty qualified as NEHashMap
import Data.HashSet qualified as HashSet
import Data.Semigroup.Foldable (toNonEmpty)
import Data.String.Interpolate (i)
import Data.Text.Extended
import Database.PG.Query.PTI qualified as PTI
import Database.PG.Query.Pool (fromPGTxErr)
import Database.PG.Query.Transaction (Query)
import Database.PG.Query.Transaction qualified as Query
import Database.PostgreSQL.LibPQ qualified as PQ
import Hasura.Backends.Postgres.DDL qualified as Postgres
import Hasura.Backends.Postgres.Execute.Types (PGExecCtxInfo (..), PGExecFrom (..), PGExecTxType (..), runPgSourceReadTx, _pecRunTx, _pscExecCtx)
import Hasura.Backends.Postgres.Instances.NativeQueries as Postgres (validateNativeQuery)
import Hasura.Backends.Postgres.SQL.Types (QualifiedObject (..), QualifiedTable)
import Hasura.Backends.Postgres.SQL.Types qualified as Postgres
import Hasura.Backends.Postgres.Types.CitusExtraTableMetadata
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.DDL.Relationship (defaultBuildArrayRelationshipInfo, defaultBuildObjectRelationshipInfo)
import Hasura.RQL.Types.Backend (Backend)
import Hasura.RQL.Types.Backend qualified as Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column (ColumnMutability (..), RawColumnInfo (..))
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.SchemaCache (askSourceConfig)
import Hasura.RQL.Types.Source.Column (SourceColumnInfo (..))
import Hasura.RQL.Types.Source.Table (SourceConstraint (..), SourceForeignKeys (..), SourceTableInfo (..), SourceTableType (..))
import Hasura.Table.Cache
import Language.GraphQL.Draft.Syntax (unDescription)

--------------------------------------------------------------------------------
-- PostgresMetadata

-- | We differentiate the handling of metadata between Citus and Vanilla
-- Postgres because Citus imposes limitations on the types of joins that it
-- permits, which then limits the types of relations that we can track.
class PostgresMetadata (pgKind :: PostgresKind) where
  -- TODO: find a better name
  validateRel ::
    (MonadError QErr m) =>
    TableCache ('Postgres pgKind) ->
    QualifiedTable ->
    Either (ObjRelDef ('Postgres pgKind)) (ArrRelDef ('Postgres pgKind)) ->
    m ()

  -- | A query for getting the list of all tables on a given data source. This
  -- is primarily used by the console to display tables for tracking etc.
  listAllTablesSql :: Query

  -- | Given 'PgExtraTableMetadata', return whether this is source "table" is a
  -- table or a view.
  tableTypeImpl :: Backend.ExtraTableMetadata ('Postgres pgKind) -> SourceTableType

  -- | A mapping from pg scalar types with clear oid equivalent to oid.
  --
  -- This is a insert order hash map so that when we invert it
  -- duplicate oids will point to a more "general" type.
  pgTypeOidMapping :: InsOrdHashMap.InsOrdHashMap Postgres.PGScalarType PQ.Oid
  pgTypeOidMapping =
    InsOrdHashMap.fromList
      $ [ (Postgres.PGSmallInt, PTI.int2),
          (Postgres.PGSerial, PTI.int4),
          (Postgres.PGInteger, PTI.int4),
          (Postgres.PGBigSerial, PTI.int8),
          (Postgres.PGBigInt, PTI.int8),
          (Postgres.PGFloat, PTI.float4),
          (Postgres.PGDouble, PTI.float8),
          (Postgres.PGMoney, PTI.numeric),
          (Postgres.PGNumeric, PTI.numeric),
          (Postgres.PGBoolean, PTI.bool),
          (Postgres.PGChar, PTI.bpchar),
          (Postgres.PGVarchar, PTI.varchar),
          (Postgres.PGText, PTI.text),
          (Postgres.PGDate, PTI.date),
          (Postgres.PGTimeStamp, PTI.timestamp),
          (Postgres.PGTimeStampTZ, PTI.timestamptz),
          (Postgres.PGTimeTZ, PTI.timetz),
          (Postgres.PGJSON, PTI.json),
          (Postgres.PGJSONB, PTI.jsonb),
          (Postgres.PGUUID, PTI.uuid),
          (Postgres.PGArray Postgres.PGSmallInt, PTI.int2_array),
          (Postgres.PGArray Postgres.PGSerial, PTI.int4_array),
          (Postgres.PGArray Postgres.PGInteger, PTI.int4_array),
          (Postgres.PGArray Postgres.PGBigSerial, PTI.int8_array),
          (Postgres.PGArray Postgres.PGBigInt, PTI.int8_array),
          (Postgres.PGArray Postgres.PGFloat, PTI.float4_array),
          (Postgres.PGArray Postgres.PGDouble, PTI.float8_array),
          (Postgres.PGArray Postgres.PGMoney, PTI.numeric_array),
          (Postgres.PGArray Postgres.PGNumeric, PTI.numeric_array),
          (Postgres.PGArray Postgres.PGBoolean, PTI.bool_array),
          (Postgres.PGArray Postgres.PGChar, PTI.char_array),
          (Postgres.PGArray Postgres.PGVarchar, PTI.varchar_array),
          (Postgres.PGArray Postgres.PGText, PTI.text_array),
          (Postgres.PGArray Postgres.PGDate, PTI.date_array),
          (Postgres.PGArray Postgres.PGTimeStamp, PTI.timestamp_array),
          (Postgres.PGArray Postgres.PGTimeStampTZ, PTI.timestamptz_array),
          (Postgres.PGArray Postgres.PGTimeTZ, PTI.timetz_array),
          (Postgres.PGArray Postgres.PGJSON, PTI.json_array),
          (Postgres.PGArray Postgres.PGJSON, PTI.jsonb_array),
          (Postgres.PGArray Postgres.PGUUID, PTI.uuid_array)
        ]

instance PostgresMetadata 'Vanilla where
  validateRel _ _ _ = pure ()
  tableTypeImpl = Postgres._petmTableType

  listAllTablesSql =
    Query.fromText
      [i|
        WITH partitions as (
          SELECT array(
            SELECT
            child.relname       AS partition
        FROM pg_inherits
            JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
            JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
          ) as names
        )
        SELECT info_schema.table_schema, info_schema.table_name
        FROM information_schema.tables as info_schema, partitions
        WHERE
          info_schema.table_schema NOT IN ('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal')
          AND NOT (info_schema.table_name = ANY (partitions.names))
        ORDER BY info_schema.table_schema, info_schema.table_name
      |]

instance PostgresMetadata 'Citus where
  validateRel ::
    forall m.
    (MonadError QErr m) =>
    TableCache ('Postgres 'Citus) ->
    QualifiedTable ->
    Either (ObjRelDef ('Postgres 'Citus)) (ArrRelDef ('Postgres 'Citus)) ->
    m ()
  validateRel tableCache sourceTable relInfo = do
    sourceTableInfo <- lookupTableInfo sourceTable
    case relInfo of
      Left (RelDef _ obj _) ->
        case obj of
          RUFKeyOn (SameTable _) -> pure ()
          RUFKeyOn (RemoteTable targetTable _) -> checkObjectRelationship sourceTableInfo targetTable
          RUManual RelManualTableConfig {} -> pure ()
          RUManual RelManualNativeQueryConfig {} -> pure ()
      Right (RelDef _ obj _) ->
        case obj of
          RUFKeyOn (ArrRelUsingFKeyOn targetTable _col) -> checkArrayRelationship sourceTableInfo targetTable
          RUManual RelManualTableConfig {} -> pure ()
          RUManual RelManualNativeQueryConfig {} -> pure ()
    where
      lookupTableInfo tableName =
        HashMap.lookup tableName tableCache
          `onNothing` throw400 NotFound ("no such table " <>> tableName)

      checkObjectRelationship sourceTableInfo targetTable = do
        targetTableInfo <- lookupTableInfo targetTable
        let notSupported = throwNotSupportedError sourceTableInfo targetTableInfo "object"
        case ( _tciExtraTableMetadata $ _tiCoreInfo sourceTableInfo,
               _tciExtraTableMetadata $ _tiCoreInfo targetTableInfo
             ) of
          (Distributed {}, Local {}) -> notSupported
          (Distributed {}, Reference {}) -> pure ()
          (Distributed {}, Distributed {}) -> pure ()
          (_, Distributed {}) -> notSupported
          (_, _) -> pure ()

      checkArrayRelationship sourceTableInfo targetTable = do
        targetTableInfo <- lookupTableInfo targetTable
        let notSupported = throwNotSupportedError sourceTableInfo targetTableInfo "array"
        case ( _tciExtraTableMetadata $ _tiCoreInfo sourceTableInfo,
               _tciExtraTableMetadata $ _tiCoreInfo targetTableInfo
             ) of
          (Distributed {}, Distributed {}) -> pure ()
          (Distributed {}, _) -> notSupported
          (_, Distributed {}) -> notSupported
          (_, _) -> pure ()

      showDistributionType :: ExtraTableMetadata -> Text
      showDistributionType = \case
        Local {} -> "local"
        Distributed {} -> "distributed"
        Reference {} -> "reference"

      throwNotSupportedError :: TableInfo ('Postgres 'Citus) -> TableInfo ('Postgres 'Citus) -> Text -> m ()
      throwNotSupportedError sourceTableInfo targetTableInfo t =
        let tciSrc = _tiCoreInfo sourceTableInfo
            tciTgt = _tiCoreInfo targetTableInfo
         in throw400
              NotSupported
              ( showDistributionType (_tciExtraTableMetadata tciSrc)
                  <> " tables ("
                  <> toTxt (_tciName tciSrc)
                  <> ") cannot have an "
                  <> t
                  <> " relationship against a "
                  <> showDistributionType (_tciExtraTableMetadata $ _tiCoreInfo targetTableInfo)
                  <> " table ("
                  <> toTxt (_tciName tciTgt)
                  <> ")"
              )

  tableTypeImpl = tableType
  listAllTablesSql =
    Query.fromText
      [i|
        WITH partitions as (
          SELECT array(
            SELECT
            child.relname       AS partition
        FROM pg_inherits
            JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
            JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
          ) as names
        )
        SELECT info_schema.table_schema, info_schema.table_name
        FROM information_schema.tables as info_schema, partitions
        WHERE
          info_schema.table_schema NOT IN ('pg_catalog', 'citus', 'information_schema', 'columnar', 'columnar_internal', 'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog', '_timescaledb_internal')
          AND NOT (info_schema.table_name = ANY (partitions.names))
          AND info_schema.table_name NOT IN ('citus_tables')
        ORDER BY info_schema.table_schema, info_schema.table_name
      |]

instance PostgresMetadata 'Cockroach where
  validateRel _ _ _ = pure ()
  tableTypeImpl = Postgres._petmTableType

  pgTypeOidMapping =
    InsOrdHashMap.fromList
      [ (Postgres.PGInteger, PTI.int8),
        (Postgres.PGSerial, PTI.int8),
        (Postgres.PGJSON, PTI.jsonb)
      ]
      `InsOrdHashMap.union` pgTypeOidMapping @'Vanilla

  listAllTablesSql =
    Query.fromText
      [i|
        WITH partitions as (
          SELECT array(
            SELECT
            child.relname       AS partition
        FROM pg_inherits
            JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
            JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
          ) as names
        )
        SELECT info_schema.table_schema, info_schema.table_name
        FROM information_schema.tables as info_schema, partitions
        WHERE
          info_schema.table_schema NOT IN ('pg_catalog', 'crdb_internal', 'information_schema', 'columnar', 'guest', 'INFORMATION_SCHEMA', 'sys', 'db_owner', 'db_securityadmin', 'db_accessadmin', 'db_backupoperator', 'db_ddladmin', 'db_datawriter', 'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog', '_timescaledb_internal', 'pg_extension')
          AND NOT (info_schema.table_name = ANY (partitions.names))
        ORDER BY info_schema.table_schema, info_schema.table_name
      |]

----------------------------------------------------------------
-- BackendMetadata instance

instance
  ( Backend ('Postgres pgKind),
    PostgresMetadata pgKind,
    Postgres.FetchTableMetadata pgKind,
    Postgres.FetchFunctionMetadata pgKind,
    Postgres.ToMetadataFetchQuery pgKind
  ) =>
  BackendMetadata ('Postgres pgKind)
  where
  prepareCatalog = Postgres.prepareCatalog
  buildComputedFieldInfo = Postgres.buildComputedFieldInfo
  fetchAndValidateEnumValues = Postgres.fetchAndValidateEnumValues
  resolveSourceConfig = Postgres.resolveSourceConfig
  resolveDatabaseMetadata _ = Postgres.resolveDatabaseMetadata
  parseBoolExpOperations = Postgres.parseBoolExpOperations
  buildArrayRelationshipInfo _ = defaultBuildArrayRelationshipInfo
  buildObjectRelationshipInfo _ = defaultBuildObjectRelationshipInfo
  buildFunctionInfo = Postgres.buildFunctionInfo
  updateColumnInEventTrigger = Postgres.updateColumnInEventTrigger
  parseCollectableType = Postgres.parseCollectableType
  postDropSourceHook = Postgres.postDropSourceHook
  validateRelationship = validateRel @pgKind
  buildComputedFieldBooleanExp = Postgres.buildComputedFieldBooleanExp
  validateNativeQuery = Postgres.validateNativeQuery (pgTypeOidMapping @pgKind)
  supportsBeingRemoteRelationshipTarget _ = True

  getTableInfo sourceName tableName = do
    sourceConfig <- askSourceConfig @('Postgres pgKind) sourceName

    result <-
      either throwError pure =<< runExceptT do
        _pecRunTx (_pscExecCtx sourceConfig) (PGExecCtxInfo NoTxRead InternalRawQuery)
          $ Postgres.pgFetchTableMetadata @pgKind (HashSet.singleton tableName)

    pure do
      DBTableMetadata {..} <- HashMap.lookup tableName result

      let convertColumn :: RawColumnInfo ('Postgres pgKind) -> SourceColumnInfo ('Postgres pgKind)
          convertColumn RawColumnInfo {..} =
            SourceColumnInfo
              { _sciName = rciName,
                _sciType = rciType,
                _sciNullable = rciIsNullable,
                _sciDescription = fmap unDescription rciDescription,
                _sciInsertable = _cmIsInsertable rciMutability,
                _sciUpdatable = _cmIsUpdatable rciMutability,
                _sciValueGenerated = Nothing
              }

          convertForeignKeys :: HashSet (ForeignKeyMetadata ('Postgres pgKind)) -> SourceForeignKeys ('Postgres pgKind)
          convertForeignKeys foreignKeys = SourceForeignKeys $ HashMap.fromList do
            ForeignKeyMetadata ForeignKey {..} <- HashSet.toList foreignKeys

            let mappings :: HashMap Postgres.PGCol Postgres.PGCol
                mappings = NEHashMap.toHashMap _fkColumnMapping

            pure (_cName _fkConstraint, SourceConstraint _fkForeignTable $ RelMapping mappings)

      pure
        SourceTableInfo
          { _stiName = tableName,
            _stiInsertable = maybe True viIsInsertable _ptmiViewInfo,
            _stiUpdatable = maybe True viIsUpdatable _ptmiViewInfo,
            _stiDeletable = maybe True viIsDeletable _ptmiViewInfo,
            _stiForeignKeys = convertForeignKeys _ptmiForeignKeys,
            _stiPrimaryKey = fmap (toNonEmpty . _pkColumns) _ptmiPrimaryKey,
            _stiColumns = map convertColumn _ptmiColumns,
            _stiLogicalModels = [],
            _stiType = tableTypeImpl @pgKind _ptmiExtraTableMetadata,
            _stiDescription = Nothing
          }

  listAllTables sourceName = do
    sourceConfig <- askSourceConfig @('Postgres pgKind) sourceName

    results <-
      runPgSourceReadTx sourceConfig (Query.multiQE fromPGTxErr (listAllTablesSql @pgKind))
        `onLeftM` \err -> throwError (prefixQErr "failed to fetch source tables: " err)

    pure [QualifiedObject {..} | (qSchema, qName) <- results]

  listAllTrackables _ = throw500 "listAllTrackables not supported by Postgres"
