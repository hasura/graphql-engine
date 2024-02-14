import { DeepRequired } from 'ts-essentials';

import { DataSourcesAPI } from '../..';
import {
  getFetchTablesListQuery,
  fetchColumnTypesQuery,
  fetchColumnDefaultFunctions,
  isSQLFunction,
  getEstimateCountQuery,
  cascadeSqlQuery,
  getCreateTableQueries,
  getDropTableSql,
  getStatementTimeoutSql,
  getCreateSchemaSql,
  getDropSchemaSql,
  getAlterForeignKeySql,
  getCreateFKeySql,
  getDropConstraintSql,
  getRenameTableSql,
  getDropTriggerSql,
  getCreateTriggerSql,
  getViewDefinitionSql,
  getDropSql,
  getDropColumnSql,
  getAddColumnSql,
  getAddUniqueConstraintSql,
  getDropNotNullSql,
  getSetColumnDefaultSql,
  getSetNotNullSql,
  getAlterColumnTypeSql,
  getDropColumnDefaultSql,
  getRenameColumnQuery,
  fetchColumnCastsQuery,
  checkSchemaModification,
  getCreateCheckConstraintSql,
  getCreatePkSql,
  getAlterPkSql,
  getFunctionDefinitionSql,
  primaryKeysInfoSql,
  checkConstraintsSql,
  uniqueKeysSql,
  frequentlyUsedColumns,
  getFKRelations,
  deleteFunctionSql,
  getEventInvocationInfoByIDSql,
  getDatabaseInfo,
  tableIndexSql,
  getCreateIndexSql,
  getDropIndexSql,
  getTableInfo,
  getDatabaseVersionSql,
  schemaListQuery,
  getAlterTableCommentSql,
  getAlterColumnCommentSql,
  getAlterFunctionCommentSql,
  getDataTriggerInvocations,
  getDataTriggerLogsQuery,
} from './sqlUtils';
import {
  generateTableRowRequest,
  generateInsertRequest,
  generateRowsCountRequest,
  generateEditRowRequest,
  generateDeleteRowRequest,
  generateBulkDeleteRowRequest,
} from './utils';
import globals from '../../../Globals';
import { SupportedFeaturesType } from '../../types';
import {
  postgres,
  supportedFeatures as PgSupportedFeatures,
} from '../postgresql';

export const supportedFeatures: DeepRequired<SupportedFeaturesType> = {
  ...PgSupportedFeatures,
  connectDbForm: {
    ...PgSupportedFeatures?.connectDbForm,
    connectionParameters: false,
    enabled: false,
    namingConvention: false,
    extensions_schema: false,
    ssl_certificates:
      globals.consoleType === 'cloud' ||
      globals.consoleType === 'pro' ||
      globals.consoleType === 'pro-lite', // TODO: Should be allowed only if the license is active
  },
  driver: {
    name: 'cockroach',
    fetchVersion: {
      enabled: false,
    },
  },
  tables: {
    ...(PgSupportedFeatures?.tables ?? {}),
    browse: {
      enabled: true,
      aggregation: true,
      customPagination: true,
      deleteRow: true,
      editRow: true,
      bulkRowSelect: true,
    },
    modify: {
      ...(PgSupportedFeatures?.tables?.modify ?? {}),
      enabled: true,
      computedFields: false,
      triggers: false,
      customGqlRoot: true,
      setAsEnum: true,
      untrack: true,
      delete: true,
      indexes: {
        edit: false,
        view: false,
      },
    },
  },
  events: {
    triggers: {
      add: false,
      enabled: true,
    },
  },
  actions: {
    enabled: true,
    relationships: false,
  },
  functions: {
    enabled: false,
    track: {
      enabled: false,
    },
    nonTrackableFunctions: {
      enabled: true,
    },
    modify: {
      enabled: false,
      comments: {
        view: false,
        edit: false,
      },
    },
  },
};

const schemaListSql = (
  schemas?: string[]
) => `SELECT schema_name FROM information_schema.schemata WHERE
schema_name NOT IN ('information_schema', 'hdb_catalog', 'hdb_views', '_timescaledb_internal', 'crdb_internal') AND schema_name NOT LIKE 'pg\\_%'
${schemas?.length ? ` AND schema_name IN (${schemas.join(',')})` : ''}
ORDER BY schema_name ASC;`;

export const isColTypeString = (colType: string) =>
  ['text', 'varchar', 'char', 'bpchar', 'name'].includes(colType);

export const cockroach: DataSourcesAPI = {
  ...postgres,
  supportedFeatures,
  schemaListSql,
  getFetchTablesListQuery,
  generateTableRowRequest,
  generateInsertRequest,
  generateRowsCountRequest,
  generateEditRowRequest,
  generateDeleteRowRequest,
  generateBulkDeleteRowRequest,
  schemaListQuery,
  isColTypeString,
  getFunctionDefinitionSql,
  fetchColumnTypesQuery,
  fetchColumnDefaultFunctions,
  isSQLFunction,
  getEstimateCountQuery,
  cascadeSqlQuery,
  getCreateTableQueries,
  getDropTableSql,
  getStatementTimeoutSql,
  getCreateSchemaSql,
  getDropSchemaSql,
  getAlterForeignKeySql,
  getCreateFKeySql,
  getDropConstraintSql,
  getRenameTableSql,
  getDropTriggerSql,
  getCreateTriggerSql,
  getViewDefinitionSql,
  getDropSql,
  getDropColumnSql,
  getAddColumnSql,
  getAddUniqueConstraintSql,
  getDropNotNullSql,
  getSetColumnDefaultSql,
  getSetNotNullSql,
  getAlterColumnTypeSql,
  getDropColumnDefaultSql,
  getRenameColumnQuery,
  fetchColumnCastsQuery,
  checkSchemaModification,
  getCreateCheckConstraintSql,
  getCreatePkSql,
  getAlterPkSql,
  primaryKeysInfoSql,
  checkConstraintsSql,
  uniqueKeysSql,
  frequentlyUsedColumns,
  getFKRelations,
  deleteFunctionSql,
  getEventInvocationInfoByIDSql,
  getDatabaseInfo,
  tableIndexSql,
  getTableInfo,
  getDatabaseVersionSql,
  getAlterTableCommentSql,
  getAlterColumnCommentSql,
  getAlterFunctionCommentSql,
  getDataTriggerInvocations,
  getDataTriggerLogsQuery,
  createIndexSql: getCreateIndexSql,
  dropIndexSql: getDropIndexSql,
};
