import { Capabilities } from '@hasura/dc-api-types';
import { SupportedFeaturesType } from '../types';
import { DeepRequired, Path } from '@hasura/shared/types';
import get from 'lodash/get';

export const postgresCapabilities: Capabilities = {
  mutations: {
    insert: {},
    update: {},
    delete: {},
  },
  queries: {
    foreach: {},
  },
  explain: {},
  metrics: {},
  subscriptions: {},
  relationships: {},
  user_defined_functions: {},
  data_schema: {
    supports_foreign_keys: true,
    supports_primary_keys: true,
    column_nullability: 'nullable_and_non_nullable',
    // Native SQL drivers (postgres/citus/cockroach/mssql, which reuse these
    // capabilities) do NOT support schemaless/collection tables — that is a
    // Mongo/GDC feature whose capability comes from the connector. Leaving this
    // `true` made TableRow route the Track button to the Mongo "Track
    // Collection" modal for plain PostgreSQL tables, so trackTables was never
    // called and the table stayed untracked.
    supports_schemaless_tables: false,
  },
  interpolated_queries: {},
};

export const postgresSupportedFeatures: DeepRequired<SupportedFeaturesType> = {
  tables: {
    view: true,
    create: {
      enabled: true,
      arrayTypes: true,
    },
    browse: {
      enabled: true,
      aggregation: false,
      customPagination: true,
      deleteRow: true,
      editRow: true,
      bulkRowSelect: true,
    },
    insert: {
      enabled: true,
    },
    modify: {
      readOnly: false,
      enabled: true,
      columns: {
        view: true,
        edit: true,
        graphqlFieldName: true,
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
      remoteDbRelationships: {
        hostSource: true,
        referenceSource: true,
      },
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
    track: {
      enabled: true,
    },
  },
  events: {
    triggers: {
      add: true,
    },
  },
  actions: {
    relationships: true,
  },
  rawSQL: {
    tracking: true,
  },
  connectDbForm: {
    enabled: true,
    connectionParameters: true,
    databaseURL: true,
    environmentVariable: true,
    read_replicas: {
      create: true,
      edit: true,
    },
    prepared_statements: true,
    isolation_level: true,
    connectionSettings: true,
    cumulativeMaxConnections: true,
    retries: true,
    extensions_schema: true,
    pool_timeout: true,
    connection_lifetime: true,
    namingConvention: true,
    ssl_certificates: true,
  },
};

export const isFeatureSupported = (
  feature: Path<DeepRequired<SupportedFeaturesType>>,
  supportedFeatures: SupportedFeaturesType | null | undefined,
) => {
  if (!supportedFeatures) {
    return false;
  }

  return Boolean(get(supportedFeatures, feature));
};
