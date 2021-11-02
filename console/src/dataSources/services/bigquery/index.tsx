import React from 'react';
import { DeepRequired } from 'ts-essentials';
import { DataSourcesAPI } from '../..';
import { QualifiedTable } from '../../../metadata/types';
import {
  TableColumn,
  Table,
  BaseTableColumn,
  SupportedFeaturesType,
  ViolationActions,
} from '../../types';
import { generateTableRowRequest } from './utils';

const permissionColumnDataTypes = {
  character: [
    'STRING',
    'INT64',
    'NUMERIC',
    'DECIMAL',
    'BIGNUMERIC',
    'BIGDECIMAL',
    'FLOAT64',
    'INTEGER',
  ],
  numeric: [],
  dateTime: ['DATETIME', 'TIME', 'TIMESTAMP'],
  user_defined: [],
};

const supportedColumnOperators = [
  '_is_null',
  '_eq',
  '_neq',
  '_gt',
  '_lt',
  '_gte',
  '_lte',
];

const isTable = (table: Table) => {
  if (!table.table_type) return true; // todo
  return table.table_type === 'TABLE' || table.table_type === 'BASE TABLE';
};

const columnDataTypes = {
  INTEGER: 'integer',
  BIGINT: 'bigint',
  GUID: 'guid',
  JSONDTYPE: 'nvarchar',
  DATETIMEOFFSET: 'timestamp with time zone',
  NUMERIC: 'numeric',
  DATE: 'date',
  TIME: 'time',
  TEXT: 'text',
};

const operators = [
  { name: 'equals', value: '$eq', graphqlOp: '_eq' },
  { name: 'not equals', value: '$ne', graphqlOp: '_neq' },
  { name: '>', value: '$gt', graphqlOp: '_gt' },
  { name: '<', value: '$lt', graphqlOp: '_lt' },
  { name: '>=', value: '$gte', graphqlOp: '_gte' },
  { name: '<=', value: '$lte', graphqlOp: '_lte' },
];

// createSQLRegex matches one or more sql for creating view, table or functions, and extracts the type, schema, name and also if it is a partition.
// An example string it matches: CREATE TABLE myschema.user(id serial primary key, name text);
// type = table, schema = myschema, nameWithSchema = user, partition = undefined
// eslint-disable-next-line no-useless-escape
const createSQLRegex = /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>\"?\w+\"?)\.(?<nameWithSchema>\"?\w+\"?)|(?<name>\"?\w+\"?))\s*(?<partition>partition\s*of)?/gim;

export const displayTableName = (table: Table) => {
  const tableName = table.table_name;

  return isTable(table) ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const isJsonColumn = (column: BaseTableColumn): boolean => {
  return column.data_type_name === 'json' || column.data_type_name === 'jsonb';
};

export const supportedFeatures: DeepRequired<SupportedFeaturesType> = {
  driver: {
    name: 'bigquery',
    fetchVersion: {
      enabled: false,
    },
  },
  schemas: {
    create: {
      enabled: false,
    },
    delete: {
      enabled: false,
    },
  },
  tables: {
    create: {
      enabled: false,
      frequentlyUsedColumns: false,
      columnTypeSelector: false,
    },
    browse: {
      enabled: true,
      customPagination: true,
      aggregation: false,
    },
    insert: {
      enabled: false,
    },
    modify: {
      editableTableName: false,
      readOnly: false,
      comments: {
        view: false,
        edit: false,
      },
      enabled: false,
      columns: {
        view: false,
        edit: false,
        graphqlFieldName: false,
        frequentlyUsedColumns: false,
      },
      computedFields: false,
      primaryKeys: {
        view: false,
        edit: false,
      },
      foreignKeys: {
        view: false,
        edit: false,
      },
      uniqueKeys: {
        view: false,
        edit: false,
      },
      triggers: false,
      checkConstraints: {
        view: false,
        edit: false,
      },
      indexes: {
        view: false,
        edit: false,
      },
      customGqlRoot: false,
      setAsEnum: false,
      untrack: false,
      delete: false,
    },
    relationships: {
      enabled: true,
      track: false,
      remoteRelationships: false,
    },
    permissions: {
      enabled: true,
      aggregation: true,
    },
    track: {
      enabled: true,
    },
  },
  functions: {
    enabled: true,
    track: {
      enabled: false,
    },
    nonTrackableFunctions: {
      enabled: false,
    },
  },
  events: {
    triggers: {
      enabled: true,
      add: false,
    },
  },
  actions: {
    enabled: true,
    relationships: false,
  },
  rawSQL: {
    enabled: true,
    tracking: false,
    statementTimeout: false,
  },
  connectDbForm: {
    enabled: true,
    connectionParameters: true,
    databaseURL: false,
    environmentVariable: true,
    read_replicas: false,
    prepared_statements: false,
    isolation_level: false,
    connectionSettings: false,
    retries: false,
    pool_timeout: false,
    connection_lifetime: false,
    ssl_certificates: false,
  },
};

const violationActions: ViolationActions[] = [
  'restrict',
  'no action',
  'cascade',
  'set null',
  'set default',
];

export const bigquery: DataSourcesAPI = {
  isTable,
  isJsonColumn,
  displayTableName,
  operators,
  generateTableRowRequest,
  getFunctionSchema: () => {
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
  getGroupedTableComputedFields: () => {
    return { scalar: [], table: [] };
  },
  isColumnAutoIncrement: () => {
    return false;
  },
  getTableSupportedQueries: () => {
    // since only subscriptions and queries are supported on MSSQL atm.
    return ['select'];
  },
  getColumnType: (col: TableColumn) => col.data_type_name ?? col.data_type,
  arrayToPostgresArray: () => {
    return '';
  },
  schemaListSql: () => {
    return '';
  },
  parseColumnsInfoResult: () => {
    return {};
  },
  columnDataTypes,
  getFetchTablesListQuery: ({ schemas, tables }) => {
    let datasets = [];
    if (schemas) {
      datasets = schemas;
    } else {
      datasets = tables.map(t => t.table_schema);
    }

    const query = (dataset: string) => `
    select
    t.table_schema as table_schema,
    t.table_name as table_name,
    t.table_type as table_type, 
    opts.option_value as comment,
    CONCAT("[", c.json_data ,"]") as columns  
    FROM ${dataset}.INFORMATION_SCHEMA.TABLES as t
    LEFT JOIN 
    (
    with x as (
        select table_name, table_schema, column_name, ordinal_position, is_nullable, data_type from ${dataset}.INFORMATION_SCHEMA.COLUMNS
    ) select x.table_name as table_name, x.table_schema as table_schema, STRING_AGG(TO_JSON_STRING(x)) as json_data from x group by x.table_name,x.table_schema
    ) as c
    ON c.table_name = t.table_name and t.table_schema = c.table_schema
    LEFT JOIN ${dataset}.INFORMATION_SCHEMA.TABLE_OPTIONS as opts
    ON opts.table_name = t.table_name and opts.table_schema = t.table_schema and opts.option_name = "description"
  `;

    return datasets
      .map(dataset => {
        return query(dataset);
      })
      .join('union all');
  },
  commonDataTypes: [],
  fetchColumnTypesQuery: '',
  fetchColumnDefaultFunctions: () => {
    return '';
  },
  isSQLFunction: () => {
    return false;
  },
  getEstimateCountQuery: () => {
    return '';
  },
  isColTypeString: () => {
    return false;
  },
  cascadeSqlQuery: () => {
    return '';
  },
  dependencyErrorCode: '',
  getCreateTableQueries: () => {
    return [];
  },
  getDropTableSql: () => {
    return '';
  },
  createSQLRegex,
  getDropSchemaSql: (schema: string) => {
    return `drop schema ${schema};`;
  },
  getCreateSchemaSql: (schema: string) => {
    return `create schema ${schema};`;
  },
  isTimeoutError: () => {
    return false;
  },
  getAlterForeignKeySql: () => {
    return '';
  },
  getCreateFKeySql: () => {
    return '';
  },
  getDropConstraintSql: () => {
    return '';
  },
  getRenameTableSql: () => {
    return '';
  },
  getDropTriggerSql: () => {
    return '';
  },
  getCreateTriggerSql: () => {
    return '';
  },
  getDropSql: () => {
    return '';
  },
  getViewDefinitionSql: () => {
    return '';
  },
  getDropColumnSql: () => {
    return '';
  },
  getAddColumnSql: () => {
    return '';
  },
  getAddUniqueConstraintSql: () => {
    return '';
  },
  getDropNotNullSql: () => {
    return '';
  },
  getSetCommentSql: () => {
    return '';
  },
  getSetColumnDefaultSql: () => {
    return '';
  },
  getSetNotNullSql: () => {
    return '';
  },
  getAlterColumnTypeSql: () => {
    return '';
  },
  getDropColumnDefaultSql: () => {
    return '';
  },
  getRenameColumnQuery: () => {
    return '';
  },
  fetchColumnCastsQuery: '',
  checkSchemaModification: () => {
    return false;
  },
  getCreateCheckConstraintSql: () => {
    return '';
  },
  getCreatePkSql: () => {
    return '';
  },
  getAlterPkSql: () => {
    return '';
  },
  getFunctionDefinitionSql: null,
  primaryKeysInfoSql: () => {
    return 'select []';
  },
  checkConstraintsSql: () => {
    return 'select []';
  },
  uniqueKeysSql: () => {
    return 'select []';
  },
  frequentlyUsedColumns: [],
  getFKRelations: () => {
    return 'select []';
  },
  getReferenceOption: () => {
    return '';
  },
  deleteFunctionSql: () => {
    return '';
  },
  getEventInvocationInfoByIDSql: () => {
    return '';
  },
  getDatabaseInfo: '',
  getTableInfo: (tables: QualifiedTable[]) => {
    if (!tables.length) return 'select []';

    const schemaMap = {} as Record<string, Array<string>>;
    tables.forEach(t => {
      if (!schemaMap[t.schema]) schemaMap[t.schema] = [t.name];
      else schemaMap[t.schema].push(t.name);
    });
    let query = '';
    Object.keys(schemaMap).forEach((schema, index) => {
      query += ` select 
      table_name, 
      table_schema, 
      case
        when table_type = 'VIEW' then 'view'
        else 'table'
      end as table_type 
      from  ${schema}.INFORMATION_SCHEMA.TABLES where table_name in (${schemaMap[
        schema
      ]
        .map(t => `'${t}'`)
        .join(',')})`;
      if (index !== Object.keys(schemaMap).length - 1) query += ` union all`;
    });
    return query;
  },
  supportedFeatures,
  // getDatabaseVersionSql: 'SELECT @@VERSION;',
  getDatabaseVersionSql: '', // TODO fixme;
  permissionColumnDataTypes,
  viewsSupported: false,
  supportedColumnOperators,
  violationActions,
};
