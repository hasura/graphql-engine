import { DataSourcesAPI } from '../..';

export const mysql: DataSourcesAPI = {
  isTable: () => {
    throw new Error('not implemented');
  },
  displayTableName: () => {
    throw new Error('not implemented');
  },
  getFunctionSchema: () => {
    throw new Error('not implemented');
  },
  getFunctionName: () => {
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
  dependecyErrorCode: '',
  getCreateTableQueries: () => {
    throw new Error('not implemented');
  },
  getDropTableSql: () => {
    throw new Error('not implemented');
  },
  createSQLRegex: /?/,
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
  getAlterForeignKeySql: () => {
    throw new Error('not implemented');
  },
  getCreateFKeySql: () => {
    throw new Error('not implemented');
  },
  getDropConstraintSql: () => {
    throw new Error('not implemented');
  },
  getRenameTableSql: () => {
    throw new Error('not implemented');
  },
  getDropTriggerSql: () => {
    throw new Error('not implemented');
  },
  getCreateTriggerSql: () => {
    throw new Error('not implemented');
  },
  getDropSql: () => {
    throw new Error('not implemented');
  },
  getViewDefinitionSql: () => {
    throw new Error('not implemented');
  },
  getDropColumnSql: () => {
    throw new Error('not implemented');
  },
  getAddColumnSql: () => {
    throw new Error('not implemented');
  },
  getAddUniqueConstraintSql: () => {
    throw new Error('not implemented');
  },
  getDropNullSql: () => {
    throw new Error('not implemented');
  },
  getSetCommentSql: () => {
    throw new Error('not implemented');
  },
  getSetColumnDefaultSql: () => {
    throw new Error('not implemented');
  },
  getSetNullSql: () => {
    throw new Error('not implemented');
  },
  getAlterColumnTypeSql: () => {
    throw new Error('not implemented');
  },
  getDropColumnDefaultSql: () => {
    throw new Error('not implemented');
  },
  getRenameColumnQuery: () => {
    throw new Error('not implemented');
  },
  fetchColumnCastsQuery: '',
  checkSchemaModification: () => {
    throw new Error('not implemented');
  },
  getCreateCheckConstraintSql: () => {
    throw new Error('not implemented');
  },
  getCreatePkSql: () => {
    throw new Error('not implemented');
  },
};
