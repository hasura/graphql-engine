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
  getDropSchemaSql,
  getCreateSchemaSql,
  getDropTableSql,
  isColTypeString,
  getCreateTableQueries,
  getFetchTablesListQuery,
  getFKRelations,
  primaryKeysInfoSql,
  uniqueKeysSql,
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

const columnDataTypes = {
  INTEGER: 'integer',
  SERIAL: 'serial',
  BIGINT: 'bigint',
  JSONDTYPE: 'json',
  TIMESTAMP: 'timestamp stored as UTC',
  DATETIME: 'time with time zone',
  NUMERIC: 'numeric',
  DATE: 'date',
  BOOLEAN: 'boolean',
  TEXT: 'text',
  TIME: 'time',
};

const commonDataTypes = [
  {
    name: 'Integer',
    value: 'integer',
    description: 'signed four-byte integer',
  },
  {
    name: 'Integer (auto-increment)',
    value: 'serial',
    description: 'autoincrementing unsigned bigint',
  },
  {
    name: 'Text',
    value: 'text',
    description: 'variable-length character string',
  },
  {
    name: 'Boolean',
    value: 'boolean',
    description: 'logical Boolean (true/false)',
  },
  {
    name: 'Numeric',
    value: 'numeric',
    description: 'exact numeric of selected precision (decimal(10,0)',
  },
  {
    name: 'Timestamp',
    value: 'timestamp',
    description: 'date and time, UTC time zone',
  },
  {
    name: 'Time',
    value: 'time',
    description: 'time of day (no time zone)',
  },
  {
    name: 'Date',
    value: 'date',
    description: 'calendar date (year, month, day)',
  },
  {
    name: 'Datetime',
    value: 'datetime',
    description: 'time with time zone',
  },
  {
    name: 'Big Integer',
    value: 'bigint',
    description: 'signed eight-byte integer',
  },
  {
    name: 'JSON',
    value: 'json',
    description: 'json format',
  },
];

const createSQLRegex = /create\s*((?:|or\s*replace)\s*view|\s*(table|function))\s*(?:\s*if*\s*not\s*exists\s*)?(((\`?\w+\`?)\.(\`?\w+\`?))|(\`?\w+\`?))/; // eslint-disable-line

// Change this to the format to what is present on the postgres side
export const mysql: DataSourcesAPI = {
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
    // NOTE: not necessary for MySQL
    throw new Error('not implemented');
  },
  additionalColumnsInfoQuery: () => {
    throw new Error('not implemented');
  },
  parseColumnsInfoResult: () => {
    throw new Error('not implemented');
  },
  getFetchTablesListQuery,
  fetchColumnDefaultFunctions: () => {
    throw new Error('not implemented');
  },
  isSQLFunction: () => {
    throw new Error('not implemented');
  },
  getEstimateCountQuery: () => {
    throw new Error('not implemented');
  },
  getStatementTimeoutSql: () => {
    throw new Error('not implemented');
  },
  isTimeoutError: () => {
    throw new Error('not implemented');
  },
  getViewDefinitionSql: () => {
    throw new Error('not implemented');
  },
  cascadeSqlQuery: () => {
    throw new Error('not implemented');
  },
  schemaList: {},
  dependencyErrorCode: '',
  fetchColumnTypesQuery: '',
  fetchColumnCastsQuery: '',
  columnDataTypes,
  commonDataTypes,
  createSQLRegex,
  isTable,
  displayTableName,
  getCreateTableQueries,
  isColTypeString,
  getDropTableSql,
  getCreateSchemaSql,
  getDropSchemaSql,
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
  frequentlyUsedColumns: [],
  primaryKeysInfoSql,
  uniqueKeysSql,
  checkConstraintsSql: undefined, // todo
  getFKRelations,
};
