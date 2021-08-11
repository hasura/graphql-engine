import React from 'react';
import { DataSourcesAPI } from '../..';
import { Table, TableColumn, ViolationActions } from '../../types';
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
  getAlterPkSql,
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
  schemaListSql,
  getAdditionalColumnsInfoQuerySql,
} from './sqlUtils';
import { getTableSupportedQueries } from '../postgresql';

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

const operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
];

const createSQLRegex = /create\s*((?:|or\s*replace)\s*view|\s*(table|function|view))\s*(?:\s*if*\s*not\s*exists\s*)?(((\`?\w+\`?)\.(\`?\w+\`?))|(\`?\w+\`?))/g; // eslint-disable-line

const violationActions: ViolationActions[] = [
  'restrict',
  'no action',
  'cascade',
  'set null',
  'set default',
];

export const mysql: DataSourcesAPI = {
  getFunctionSchema: () => {
    return '';
  },
  isJsonColumn: () => false,
  getFunctionDefinitionSql: () => {
    return '';
  },
  getFunctionDefinition: () => {
    return '';
  },
  getSchemaFunctions: () => {
    return [];
  },
  findFunction: () => {
    return undefined;
  },
  deleteFunctionSql: undefined,
  getGroupedTableComputedFields: () => {
    return { scalar: [], table: [] };
  },
  isColumnAutoIncrement: (column: TableColumn) => {
    return column.extra === 'auto-increment';
  },
  getTableSupportedQueries,
  getColumnType: (col: TableColumn) => {
    return col.data_type;
  },
  arrayToPostgresArray: (...args: any) => {
    return args;
  },
  getAdditionalColumnsInfoQuerySql,
  parseColumnsInfoResult: (args: any) => args,
  getFetchTablesListQuery,
  fetchColumnTypesQuery: 'select "[]"',
  fetchColumnCastsQuery: 'select "[]"',
  fetchColumnDefaultFunctions: () => 'select "[]"',
  isSQLFunction: () => false,
  getEstimateCountQuery: (schema: string, table: string) => {
    return `
SELECT
  TABLE_ROWS
FROM
  INFORMATION_SCHEMA.TABLES
WHERE
  information_schema.\`TABLES\`.\`TABLE_NAME\` = "${table}" AND
  information_schema.\`TABLES\`.\`TABLE_SCHEMA\` = ${schema};
`;
  },
  getStatementTimeoutSql: (seconds: number) =>
    `SET SESSION MAX_EXECUTION_TIME=${seconds * 1000};`,
  isTimeoutError: () => false,
  getViewDefinitionSql: viewName => `
  SELECT  VIEW_DEFINITION
    FROM    INFORMATION_SCHEMA.VIEWS
    WHERE   TABLE_NAME = "${viewName}";
  `,
  cascadeSqlQuery: () => {
    throw new Error('not implemented');
  },
  schemaListSql,
  dependencyErrorCode: '',
  columnDataTypes,
  commonDataTypes,
  operators,
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
  getAlterPkSql,
  frequentlyUsedColumns: [],
  primaryKeysInfoSql,
  uniqueKeysSql,
  checkConstraintsSql: undefined,
  tableIndexSql: undefined,
  createIndexSql: undefined,
  dropIndexSql: undefined,
  getFKRelations,
  getReferenceOption: (option: string) => option,
  getEventInvocationInfoByIDSql: undefined,
  getDatabaseInfo: '',
  permissionColumnDataTypes: null,
  viewsSupported: false,
  supportedColumnOperators: null,
  violationActions,
};
