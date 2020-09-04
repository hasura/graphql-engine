import React from 'react';
import { DataSourcesAPI } from '../..';
import { Table } from '../../types';
import {
  getAlterForeignKeySql,
  getCreateFKeySql,
  getDropConstraintSql,
  getRenameTableSql,
  getDropTriggerSql,
  getDropColumnSql,
  getAddColumnSql,
  getAddUniqueConstraintSql,
  getDropNotNullSql,
  getSetCommentSql,
  getSetColumnDefaultSql,
  getSetNotNullSql,
  getAlterColumnTypeSql,
  getDropColumnDefaultSql,
  getRenameColumnQuery,
  checkSchemaModification,
  getCreateCheckConstraintSql,
  getCreatePkSql,
  getCreateTriggerSql,
  getDropSql,
} from './sqlUtils';

export const isTable = (table: Table) => {
  return table.table_name === 'BASE TABLE';
};

export const displayTableName = (table: Table) => {
  return isTable(table) ? (
    <span>{table.table_name}</span>
  ) : (
    <i>{table.table_name}</i>
  );
};

// Change this to the format to what is present on the postgres side
export const mysql: DataSourcesAPI = {
  isTable,
  displayTableName,
  getFunctionSchema: () => {
    throw new Error('not implemented');
  },
  getFunctionDefinitionSql: () => {
    throw new Error('not implemented');
  },
  getFunctionDefinition: () => {
    throw new Error('not implemented');
  },
  getSchemaFunctions: () => {
    throw new Error('not implemented');
  },
  findFunction: () => {
    throw new Error('not implemented');
  },
  getGroupedTableComputedFields: () => {
    throw new Error('not implemented');
  },
  isColumnAutoIncrement: () => {
    throw new Error('not implemented');
  },
  getTableSupportedQueries: () => {
    throw new Error('not implemented');
  },
  getColumnType: () => {
    throw new Error('not implemented');
  },
  arrayToPostgresArray: () => {
    throw new Error('not implemented');
  },
  initQueries: {} as DataSourcesAPI['initQueries'],
  additionalColumnsInfoQuery: () => {
    throw new Error('not implemented');
  },
  parseColumnsInfoResult: () => {
    throw new Error('not implemented');
  },
  columnDataTypes: ({} as any) as DataSourcesAPI['columnDataTypes'],
  getFetchTrackedTableFkQuery: () => {
    throw new Error('not implemented');
  },
  getFetchTrackedTableReferencedFkQuery: () => {
    throw new Error('not implemented');
  },
  getFetchTablesListQuery: () => {
    throw new Error('not implemented');
  },
  commonDataTypes: [],
  fetchColumnTypesQuery: '',
  fetchColumnDefaultFunctions: () => {
    throw new Error('not implemented');
  },
  isSQLFunction: () => {
    throw new Error('not implemented');
  },
  getEstimateCountQuery: () => {
    throw new Error('not implemented');
  },
  isColTypeString: () => {
    throw new Error('not implemented');
  },
  cascadeSqlQuery: () => {
    throw new Error('not implemented');
  },
  dependencyErrorCode: '',
  getCreateTableQueries: () => {
    throw new Error('not implemented');
  },
  getDropTableSql: () => {
    throw new Error('not implemented');
  },
  createSQLRegex: new RegExp(''), // TODO
  getStatementTimeoutSql: () => {
    throw new Error('not implemented');
  },
  getDropSchemaSql: () => {
    throw new Error('not implemented');
  },
  getCreateSchemaSql: () => {
    throw new Error('not implemented');
  },
  isTimeoutError: () => {
    throw new Error('not implemented');
  },
  getViewDefinitionSql: () => {
    throw new Error('not implemented');
  },
  fetchColumnCastsQuery: '',
  getAlterForeignKeySql,
  getCreateFKeySql,
  getDropConstraintSql,
  getRenameTableSql,
  getDropTriggerSql,
  getCreateTriggerSql,
  getDropSql,
  getDropColumnSql,
  getAddColumnSql,
  getAddUniqueConstraintSql,
  getDropNotNullSql,
  getSetCommentSql,
  getSetColumnDefaultSql,
  getSetNotNullSql,
  getAlterColumnTypeSql,
  getDropColumnDefaultSql,
  getRenameColumnQuery,
  checkSchemaModification,
  getCreateCheckConstraintSql,
  getCreatePkSql,
  // primaryKeysInfoSql: '',
  // uniqueKeysSql: '',
  // checkConstraintsSql: '',
};
