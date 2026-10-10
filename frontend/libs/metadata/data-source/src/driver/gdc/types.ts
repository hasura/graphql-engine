import { DeepRequired } from '@hasura/shared/types';
import { SupportedFeaturesType } from '../types';

export const gdcSupportedFeatures: DeepRequired<SupportedFeaturesType> = {
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
      aggregation: false,
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
    tracking: false,
  },
  connectDbForm: {
    enabled: true,
    connectionParameters: true,
    databaseURL: false,
    environmentVariable: true,
    read_replicas: {
      create: false,
      edit: false,
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
