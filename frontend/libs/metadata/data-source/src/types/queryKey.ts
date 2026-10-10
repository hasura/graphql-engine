import { Table } from '@hasura/shared/types';

const GET_TABLE_ENUMS_QUERY_KEY = 'GET_TABLE_ENUMS';
const GET_TABLE_COMMENT_QUERY_KEY = 'GET_TABLE_COMMENT';
const GET_TABLE_FOREIGN_KEYS_QUERY_KEY = 'GET_TABLE_FOREIGN_KEYS';
const GET_TABLE_CHECK_CONSTRAINTS_QUERY_KEY = 'GET_TABLE_CHECK_CONSTRAINTS';
const GET_TABLES_FOREIGN_KEYS_QUERY_KEY = 'GET_TABLES_FOREIGN_KEYS';
const GET_TABLE_COLUMN_INFOS_QUERY_KEY = 'GET_TABLE_COLUMN_INFOS';
const GET_TRACKABLE_TABLES_QUERY_KEY = 'GET_TRACKABLE_TABLES';
const GET_TRACKABLE_FUNCTIONS_QUERY_KEY = 'GET_TRACKABLE_FUNCTIONS';

export const getTrackableFunctionsQueryKey = (dataSourceName: string) => {
  return [dataSourceName, GET_TRACKABLE_FUNCTIONS_QUERY_KEY];
};

export const getTrackableTablesQueryKey = (dataSourceName: string) => [
  dataSourceName,
  GET_TRACKABLE_TABLES_QUERY_KEY,
];

export const getTableEnumsQueryKey = (
  dataSourceName: string,
  tables: Table[],
) => [dataSourceName, GET_TABLE_ENUMS_QUERY_KEY, tables];

export const getTableCommentQueryKey = (
  dataSourceName: string,
  table: Table,
) => [dataSourceName, table, GET_TABLE_COMMENT_QUERY_KEY];

export const getTableForeignKeysQueryKey = (
  dataSourceName: string,
  table: Table,
) => [dataSourceName, table, GET_TABLE_FOREIGN_KEYS_QUERY_KEY];

export const getTableCheckConstraintsQueryKey = (
  dataSourceName: string,
  table: Table,
) => [dataSourceName, table, GET_TABLE_CHECK_CONSTRAINTS_QUERY_KEY];

export const getTablesForeignKeysQueryKey = (
  dataSourceName: string,
  tables: Table[],
) => [dataSourceName, GET_TABLES_FOREIGN_KEYS_QUERY_KEY, ...tables];

export const getTableColumnInfosQueryKey = (dataSourceName: string) => [
  dataSourceName,
  GET_TABLE_COLUMN_INFOS_QUERY_KEY,
];
