import React from 'react';
import { Table, TableColumn, ComputedField } from '../../types';
import { QUERY_TYPES, Operations } from '../../common';
import { PGFunction } from './types';
import { DataSourcesAPI, ColumnsInfoResult } from '../..';
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
  getSetCommentSql,
  getSetColumnDefaultSql,
  getSetNotNullSql,
  getAlterColumnTypeSql,
  getDropColumnDefaultSql,
  getRenameColumnQuery,
  fetchColumnCastsQuery,
  checkSchemaModification,
  getCreateCheckConstraintSql,
  getCreatePkSql,
  getFunctionDefinitionSql,
  primaryKeysInfoSql,
  checkConstraintsSql,
  uniqueKeysSql,
  frequentlyUsedColumns,
  getFKRelations,
  deleteFunctionSql,
  getEventInvocationInfoByIDSql,
  getDatabaseInfo,
} from './sqlUtils';

export const isTable = (table: Table) => {
  return (
    table.table_type === 'TABLE' ||
    table.table_type === 'PARTITIONED TABLE' ||
    table.table_type === 'FOREIGN TABLE'
  );
};

export const displayTableName = (table: Table) => {
  const tableName = table.table_name;

  return isTable(table) ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const getTableSupportedQueries = (table: Table) => {
  let supportedQueryTypes: Operations[];

  if (isTable(table)) {
    supportedQueryTypes = QUERY_TYPES;
  } else {
    // is View
    supportedQueryTypes = [];

    // Add insert/update permission if it is insertable/updatable as returned by pg
    if (table.view_info) {
      if (
        table.view_info.is_insertable_into === 'YES' ||
        table.view_info.is_trigger_insertable_into === 'YES'
      ) {
        supportedQueryTypes.push('insert');
      }

      supportedQueryTypes.push('select'); // to maintain order

      if (table.view_info.is_updatable === 'YES') {
        supportedQueryTypes.push('update');
        supportedQueryTypes.push('delete');
      } else {
        if (table.view_info.is_trigger_updatable === 'YES') {
          supportedQueryTypes.push('update');
        }

        if (table.view_info.is_trigger_deletable === 'YES') {
          supportedQueryTypes.push('delete');
        }
      }
    } else {
      supportedQueryTypes.push('select');
    }
  }

  return supportedQueryTypes;
};

export const getColumnType = (column: TableColumn) => {
  let columnType = column.data_type;

  if (columnType === 'USER-DEFINED') {
    columnType = column.data_type_name;
  }

  return columnType;
};

export const isColumnAutoIncrement = (column: TableColumn) => {
  const columnDefault = column.column_default;

  const autoIncrementDefaultRegex = /^nextval\('(.*)_seq'::regclass\)$/;

  return !!(
    columnDefault &&
    columnDefault.match(new RegExp(autoIncrementDefaultRegex, 'gi'))
  );
};

const arrayToPostgresArray = (arr: unknown[]) => {
  return `{${arr.join(',')}}`;
};

export const getFunctionSchema = (pgFunction: PGFunction) => {
  return pgFunction.function_schema;
};

export const getFunctionDefinition = (pgFunction: PGFunction) => {
  return pgFunction.function_definition;
};

export const getSchemaFunctions = (
  allFunctions: PGFunction[],
  fnSchema: string
) => {
  return allFunctions.filter(fn => getFunctionSchema(fn) === fnSchema);
};

export const findFunction = (
  allFunctions: PGFunction[],
  functionName: string,
  functionSchema: string
) => {
  return allFunctions.find(
    f =>
      f.function_name === functionName &&
      getFunctionSchema(f) === functionSchema
  );
};

export const getGroupedTableComputedFields = (
  table: Table,
  allFunctions: PGFunction[]
) => {
  const groupedComputedFields: {
    scalar: ComputedField[];
    table: ComputedField[];
  } = { scalar: [], table: [] };

  table.computed_fields.forEach(computedField => {
    const computedFieldFnDef = computedField.definition.function;
    const computedFieldFn = findFunction(
      allFunctions,
      computedFieldFnDef.name,
      computedFieldFnDef.schema
    );

    if (computedFieldFn && computedFieldFn.return_type_type === 'b') {
      groupedComputedFields.scalar.push(computedField);
    } else {
      groupedComputedFields.table.push(computedField);
    }
  });

  return groupedComputedFields;
};

const schemaListSql = `SELECT schema_name FROM information_schema.schemata WHERE
schema_name NOT IN ('information_schema', 'pg_catalog', 'hdb_catalog', 'hdb_views', 'pg_temp_1', 'pg_toast_temp_1', 'pg_toast')
ORDER BY schema_name ASC;`;

const getAdditionalColumnsInfoQuerySql = (
  schemaName: string
) => `SELECT column_name, table_name, is_generated, is_identity, identity_generation
  FROM information_schema.columns where table_schema = '${schemaName}';`;

type ColumnsInfoPayload = {
  column_name: string;
  table_name: string;
  is_generated: string;
  is_identity: string;
  identity_generation: 'ALWAYS' | 'BY DEFAULT' | null;
};

const parseColumnsInfoResult = (data: string[][]) => {
  const formattedData: ColumnsInfoPayload[] = data.slice(1).map(
    arr =>
      ({
        column_name: arr[0],
        table_name: arr[1],
        is_generated: arr[2],
        is_identity: arr[3],
        identity_generation: arr[4] === 'NULL' ? null : arr[4],
      } as ColumnsInfoPayload)
  );

  let columnsInfo: ColumnsInfoResult = {};
  formattedData
    .filter(
      (info: ColumnsInfoPayload) =>
        info.is_generated !== 'NEVER' || info.is_identity !== 'NO'
    )
    .forEach(
      ({
        column_name,
        table_name,
        is_generated,
        is_identity,
        identity_generation,
      }) => {
        columnsInfo = {
          ...columnsInfo,
          [table_name]: {
            ...columnsInfo[table_name],
            [column_name]: {
              is_generated: is_generated !== 'NEVER',
              is_identity: is_identity !== 'NO',
              identity_generation,
            },
          },
        };
      }
    );
  return columnsInfo;
};

const columnDataTypes = {
  INTEGER: 'integer',
  SERIAL: 'serial',
  BIGINT: 'bigint',
  BIGSERIAL: 'bigserial',
  UUID: 'uuid',
  JSONDTYPE: 'json',
  JSONB: 'jsonb',
  TIMESTAMP: 'timestamp with time zone',
  TIME: 'time with time zone',
  NUMERIC: 'numeric',
  DATE: 'date',
  TIMETZ: 'timetz',
  BOOLEAN: 'boolean',
  TEXT: 'text',
  ARRAY: 'ARRAY',
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
    description: 'autoincrementing four-byte integer',
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
    description: 'exact numeric of selected precision',
  },
  {
    name: 'Timestamp',
    value: 'timestamptz',
    description: 'date and time, including time zone',
  },
  {
    name: 'Time',
    value: 'timetz',
    description: 'time of day (no time zone)',
  },
  {
    name: 'Date',
    value: 'date',
    description: 'calendar date (year, month, day)',
  },
  {
    name: 'UUID',
    value: 'uuid',
    description: 'universal unique identifier',
  },
  {
    name: 'JSONB',
    value: 'jsonb',
    description: 'binary format JSON data',
  },
  {
    name: 'Big Integer',
    value: 'bigint',
    description: 'signed eight-byte integer',
  },
  {
    name: 'Big Integer (auto-increment)',
    value: 'bigserial',
    description: 'autoincrementing eight-byte integer',
  },
];

export const isColTypeString = (colType: string) =>
  ['text', 'varchar', 'char', 'bpchar', 'name'].includes(colType);

const dependencyErrorCode = '2BP01'; // pg dependent error > https://www.postgresql.org/docs/current/errcodes-appendix.html

const createSQLRegex = /create\s*(?:|or\s*replace)\s*(view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((\"?\w+\"?)\.(\"?\w+\"?)|(\"?\w+\"?))/g; // eslint-disable-line

const isTimeoutError = (error: {
  code: string;
  internal?: { error?: { message?: string } };
  message?: { error?: string; internal?: { error?: { message?: string } } };
}) => {
  if (error.internal && error.internal.error) {
    return !!error.internal?.error?.message?.includes('statement timeout');
  }

  if (error.message && error.message.error === 'postgres query error') {
    if (error.message.internal) {
      return !!error.message.internal.error?.message?.includes(
        'statement timeout'
      );
    }
    return error.message.error.includes('statement timeout');
  }

  return false;
};

// const modifyArrayType = (colType: string, displayName: string) => {
//   if (displayName === columnDataTypes.ARRAY) {
//     return `${colType.replace('_', '')}[]`;
//   }
//   return colType;
// };

const getReferenceOption = (opt: string) => {
  switch (opt) {
    case 'a':
      return 'no action';
    case 'r':
      return 'restrict';
    case 'c':
      return 'cascade';
    case 'n':
      return 'set null';
    case 'd':
      return 'set default';
    default:
      return '';
  }
};

export const postgres: DataSourcesAPI = {
  isTable,
  displayTableName,
  getFunctionSchema,
  getFunctionDefinition,
  getSchemaFunctions,
  findFunction,
  getGroupedTableComputedFields,
  isColumnAutoIncrement,
  getTableSupportedQueries,
  getColumnType,
  arrayToPostgresArray,
  schemaListSql,
  getAdditionalColumnsInfoQuerySql,
  parseColumnsInfoResult,
  columnDataTypes,
  getFetchTablesListQuery,
  commonDataTypes,
  fetchColumnTypesQuery,
  fetchColumnDefaultFunctions,
  isSQLFunction,
  getEstimateCountQuery,
  isColTypeString,
  cascadeSqlQuery,
  dependencyErrorCode,
  getCreateTableQueries,
  getDropTableSql,
  createSQLRegex,
  getStatementTimeoutSql,
  getDropSchemaSql,
  getCreateSchemaSql,
  isTimeoutError,
  getAlterForeignKeySql,
  getCreateFKeySql,
  getDropConstraintSql,
  getRenameTableSql,
  getDropTriggerSql,
  getCreateTriggerSql,
  getDropSql,
  getViewDefinitionSql,
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
  fetchColumnCastsQuery,
  checkSchemaModification,
  getCreateCheckConstraintSql,
  getCreatePkSql,
  getFunctionDefinitionSql,
  primaryKeysInfoSql,
  checkConstraintsSql,
  uniqueKeysSql,
  frequentlyUsedColumns,
  getFKRelations,
  getReferenceOption,
  deleteFunctionSql,
  getEventInvocationInfoByIDSql,
  getDatabaseInfo,
};
