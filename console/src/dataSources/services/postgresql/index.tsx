import React from 'react';
import { DeepRequired } from 'ts-essentials';

import {
  Table,
  TableColumn,
  ComputedField,
  SupportedFeaturesType,
  BaseTableColumn,
  ViolationActions,
  IndexType,
} from '../../types';
import { QUERY_TYPES, Operations } from '../../common';
import { PGFunction } from './types';
import { DataSourcesAPI, ColumnsInfoResult } from '../..';
import {
  generateTableRowRequest,
  generateInsertRequest,
  generateRowsCountRequest,
  generateEditRowRequest,
  generateDeleteRowRequest,
  generateBulkDeleteRowRequest,
} from './utils';
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
} from './sqlUtils';
import globals from '../../../Globals';

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

export const isFunctionCompatibleToTable = (
  pgFunction: PGFunction,
  tableName: string,
  tableSchema: string
) => {
  const inputArgTypes = pgFunction?.input_arg_types || [];

  let hasTableRowInArguments = false;
  let hasUnsupportedArguments = false;

  inputArgTypes.forEach(inputArgType => {
    if (!hasTableRowInArguments) {
      hasTableRowInArguments =
        inputArgType.name === tableName && inputArgType.schema === tableSchema;
    }

    if (!hasUnsupportedArguments) {
      hasUnsupportedArguments =
        inputArgType.type !== 'c' && inputArgType.type !== 'b';
    }
  });

  return hasTableRowInArguments && !hasUnsupportedArguments;
};

export const getSchemaFunctions = (
  allFunctions: PGFunction[],
  fnSchema: string,
  tableName: string,
  tableSchema: string
) => {
  return allFunctions.filter(
    fn =>
      getFunctionSchema(fn) === fnSchema &&
      isFunctionCompatibleToTable(fn, tableName, tableSchema)
  );
};

export const isJsonColumn = (column: BaseTableColumn): boolean => {
  return column.data_type === 'json' || column.data_type === 'jsonb';
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

export const getComputedFieldFunction = (
  computedField: ComputedField,
  allFunctions: PGFunction[]
) => {
  const computedFieldFnDef = computedField.definition.function;
  return findFunction(
    allFunctions,
    computedFieldFnDef.name,
    computedFieldFnDef.schema
  );
};

const schemaListSql = (
  schemas?: string[]
) => `SELECT schema_name FROM information_schema.schemata WHERE
schema_name NOT IN ('information_schema', 'hdb_catalog', 'hdb_views') AND schema_name NOT LIKE 'pg_%'
${schemas?.length ? ` AND schema_name IN (${schemas.join(',')})` : ''}
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

const operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: 'in', value: '$in', graphqlOp: '_in', defaultValue: '[]' },
  { name: 'not in', value: '$nin', graphqlOp: '_nin', defaultValue: '[]' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
  { name: 'like', value: '$like', graphqlOp: '_like', defaultValue: '%%' },
  {
    name: 'not like',
    value: '$nlike',
    graphqlOp: '_nlike',
    defaultValue: '%%',
  },
  {
    name: 'like (case-insensitive)',
    value: '$ilike',
    graphqlOp: '_ilike',
    defaultValue: '%%',
  },
  {
    name: 'not like (case-insensitive)',
    value: '$nilike',
    graphqlOp: '_nilike',
    defaultValue: '%%',
  },
  { name: 'similar', value: '$similar', graphqlOp: '_similar' },
  { name: 'not similar', value: '$nsimilar', graphqlOp: '_nsimilar' },

  {
    name: '~',
    value: '$regex',
    graphqlOp: '_regex',
  },
  {
    name: '~*',
    value: '$iregex',
    graphqlOp: '_iregex',
  },
  {
    name: '!~',
    value: '$nregex',
    graphqlOp: '_nregex',
  },
  {
    name: '!~*',
    value: '$niregex',
    graphqlOp: '_niregex',
  },
];

export const isColTypeString = (colType: string) =>
  ['text', 'varchar', 'char', 'bpchar', 'name'].includes(colType);

const dependencyErrorCode = '2BP01'; // pg dependent error > https://www.postgresql.org/docs/current/errcodes-appendix.html

const createSQLRegex = /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>\"?\w+\"?)\.(?<nameWithSchema>\"?\w+\"?)|(?<name>\"?\w+\"?))\s*(?<partition>partition\s*of)?/gim; // eslint-disable-line

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

const permissionColumnDataTypes = {
  boolean: ['boolean'],
  character: ['character', 'character varying', 'text', 'citext'],
  dateTime: [
    'timestamp',
    'timestamp with time zone',
    'timestamp without time zone',
    'date',
    'time',
    'time with time zone',
    'time without time zone',
    'interval',
  ],
  geometry: ['geometry'],
  geography: ['geography'],
  json: ['json'],
  jsonb: ['jsonb'],
  numeric: [
    'smallint',
    'integer',
    'bigint',
    'decimal',
    'numeric',
    'real',
    'double precision',
  ],
  uuid: ['uuid'],
  user_defined: [], // default for all other types
};

const indexFormToolTips = {
  unique:
    'Causes the system to check for duplicate values in the table when the index is created (if data already exist) and each time data is added',
  indexName:
    'The name of the index to be created. No schema name can be included here; the index is always created in the same schema as its parent table',
  indexType:
    'Only B-Tree, GiST, GIN and BRIN support multi-column indexes on PostgreSQL',
  indexColumns:
    'In PostgreSQL, at most 32 fields can be provided while creating an index',
};

const indexTypes: Record<string, IndexType> = {
  BRIN: 'brin',
  'SP-GIST': 'spgist',
  GiST: 'gist',
  HASH: 'hash',
  'B-Tree': 'btree',
  GIN: 'gin',
};

const supportedIndex = {
  multiColumn: ['brin', 'gist', 'btree', 'gin'],
  singleColumn: ['hash', 'spgist'],
};
export const supportedFeatures: DeepRequired<SupportedFeaturesType> = {
  driver: {
    name: 'postgres',
    fetchVersion: {
      enabled: true,
    },
  },
  schemas: {
    create: {
      enabled: true,
    },
    delete: {
      enabled: true,
    },
  },
  tables: {
    create: {
      enabled: true,
      frequentlyUsedColumns: true,
      columnTypeSelector: true,
    },
    browse: {
      enabled: true,
      aggregation: true,
      customPagination: false,
    },
    insert: {
      enabled: true,
    },
    modify: {
      readOnly: false,
      enabled: true,
      editableTableName: true,
      comments: {
        view: true,
        edit: true,
      },
      columns: {
        view: true,
        edit: true,
        graphqlFieldName: true,
        frequentlyUsedColumns: true,
      },
      computedFields: true,
      primaryKeys: {
        view: true,
        edit: true,
      },
      foreignKeys: {
        view: true,
        edit: true,
      },
      uniqueKeys: {
        view: true,
        edit: true,
      },
      triggers: true,
      checkConstraints: {
        view: true,
        edit: true,
      },
      indexes: {
        view: true,
        edit: true,
      },
      customGqlRoot: true,
      setAsEnum: true,
      untrack: true,
      delete: true,
    },
    relationships: {
      enabled: true,
      remoteRelationships: true,
      track: true,
    },
    permissions: {
      enabled: true,
      aggregation: true,
    },
    track: {
      enabled: false,
    },
  },
  functions: {
    enabled: true,
    track: {
      enabled: true,
    },
    nonTrackableFunctions: {
      enabled: true,
    },
  },
  events: {
    triggers: {
      enabled: true,
      add: true,
    },
  },
  actions: {
    enabled: true,
    relationships: true,
  },
  rawSQL: {
    enabled: true,
    tracking: true,
    statementTimeout: true,
  },
  connectDbForm: {
    enabled: true,
    connectionParameters: true,
    databaseURL: true,
    environmentVariable: true,
    read_replicas: true,
    prepared_statements: true,
    isolation_level: true,
    connectionSettings: true,
    retries: true,
    pool_timeout: true,
    connection_lifetime: true,
    ssl_certificates:
      globals.consoleType === 'cloud' || globals.consoleType === 'pro',
  },
};

const violationActions: ViolationActions[] = [
  'restrict',
  'no action',
  'cascade',
  'set null',
  'set default',
];

const defaultRedirectSchema = 'public';

const getPartitionDetailsSql = (tableName: string, tableSchema: string) => {
  return `SELECT
  nmsp_parent.nspname AS parent_schema,
  parent.relname      AS parent_table,
  child.relname       AS partition_name,
  nmsp_child.nspname  AS partition_schema,
  pg_catalog.pg_get_expr(child.relpartbound, child.oid) AS partition_def,
  pg_catalog.pg_get_partkeydef(parent.oid) AS partition_key
FROM pg_inherits
  JOIN pg_class parent            ON pg_inherits.inhparent = parent.oid
  JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
  JOIN pg_namespace nmsp_parent   ON nmsp_parent.oid  = parent.relnamespace
  JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
WHERE nmsp_child.nspname = '${tableSchema}'
AND   parent.relname = '${tableName}';`;
};

export const postgres: DataSourcesAPI = {
  isTable,
  isJsonColumn,
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
  getAlterPkSql,
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
  tableIndexSql,
  createIndexSql: getCreateIndexSql,
  dropIndexSql: getDropIndexSql,
  indexFormToolTips,
  indexTypes,
  supportedIndex,
  getTableInfo,
  operators,
  generateTableRowRequest,
  getDatabaseVersionSql,
  permissionColumnDataTypes,
  viewsSupported: true,
  supportedColumnOperators: null,
  supportedFeatures,
  violationActions,
  defaultRedirectSchema,
  generateInsertRequest,
  generateRowsCountRequest,
  getPartitionDetailsSql,
  generateEditRowRequest,
  generateDeleteRowRequest,
  generateBulkDeleteRowRequest,
};
