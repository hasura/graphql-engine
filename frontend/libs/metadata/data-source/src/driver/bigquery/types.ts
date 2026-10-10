import { DeepRequired } from '@hasura/shared/types';
import { SupportedFeaturesType } from '../types';
import { Capabilities } from '@hasura/dc-api-types';
import { TableColumn } from '../types';

export type BigQueryTable = { name: string; dataset: string };

export const DataTypeToSQLTypeMap: Record<
  TableColumn['consoleDataType'],
  string[]
> = {
  boolean: ['BOOL'],
  string: ['STRING'],
  number: [],
  integer: ['INT64'],
  text: [],
  json: ['JSON'],
  float: ['NUMERIC', 'DECIMAL', 'BIGNUMERIC', 'BIGDECIMAL', 'FLOAT64'],
  timestamp: ['DATETIME', 'TIMESTAMP'],
  geography: [],
  uuid: [],
  date: ['DATE'],
  time: ['TIME'],
  array: [],
};

export const DataTypeScalars = Object.values(DataTypeToSQLTypeMap).flat();

export const columnDataTypes = {
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

export const bigQueryCapabilities: Capabilities = {
  queries: {
    foreach: {},
  },
  relationships: {},
  data_schema: {
    supports_foreign_keys: false,
  },
  interpolated_queries: {},
};

export const bigquerySupportedFeatures: DeepRequired<SupportedFeaturesType> = {
  tables: {
    view: true,
    create: {
      enabled: false,
      arrayTypes: false,
    },
    browse: {
      enabled: true,
      customPagination: true,
      aggregation: false,
      deleteRow: false,
      editRow: false,
      bulkRowSelect: false,
    },
    insert: {
      enabled: false,
    },
    modify: {
      readOnly: false,
      enabled: false,
      columns: {
        view: false,
        edit: false,
        graphqlFieldName: false,
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
      remoteDbRelationships: {
        hostSource: true,
        referenceSource: true,
      },
      remoteRelationships: true,
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
    track: {
      enabled: false,
    },
  },
  events: {
    triggers: {
      add: false,
    },
  },
  actions: {
    relationships: false,
  },
  rawSQL: {
    tracking: true,
  },
  connectDbForm: {
    enabled: true,
    connectionParameters: true,
    databaseURL: false,
    environmentVariable: true,
    read_replicas: {
      create: true,
      edit: true,
    },
    prepared_statements: false,
    isolation_level: false,
    connectionSettings: false,
    retries: false,
    cumulativeMaxConnections: false,
    extensions_schema: false,
    pool_timeout: false,
    connection_lifetime: false,
    ssl_certificates: false,
    namingConvention: false,
  },
};
