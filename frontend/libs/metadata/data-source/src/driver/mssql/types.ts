import { DeepRequired } from '@hasura/shared/types';
import { SupportedFeaturesType } from '../types';

export type MssqlTable = { schema: string; name: string };

export const mssqlSupportedFeatures: DeepRequired<SupportedFeaturesType> = {
  tables: {
    view: true,
    create: {
      enabled: true,
      arrayTypes: false,
    },
    browse: {
      enabled: true,
      customPagination: true,
      aggregation: true,
      deleteRow: false,
      editRow: false,
      bulkRowSelect: false,
    },
    insert: {
      enabled: false,
    },
    modify: {
      enabled: true,
      columns: {
        view: true,
        edit: true,
        graphqlFieldName: false,
      },
      readOnly: false,
      computedFields: false,
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
      triggers: false,
      checkConstraints: {
        view: true,
        edit: false,
      },
      indexes: {
        view: false,
        edit: false,
      },
      customGqlRoot: true,
      setAsEnum: false,
      untrack: true,
      delete: true,
    },
    relationships: {
      enabled: true,
      track: true,
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
      enabled: false,
    },
  },
  functions: {
    track: {
      enabled: false,
    },
  },
  events: {
    triggers: {
      add: true,
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
    connectionParameters: false,
    databaseURL: true,
    environmentVariable: true,
    read_replicas: {
      create: true,
      edit: true,
    },
    prepared_statements: false,
    isolation_level: false,
    connectionSettings: true,
    retries: false,
    cumulativeMaxConnections: true,
    extensions_schema: false,
    pool_timeout: false,
    connection_lifetime: false,
    ssl_certificates: false,
    namingConvention: false,
  },
};
