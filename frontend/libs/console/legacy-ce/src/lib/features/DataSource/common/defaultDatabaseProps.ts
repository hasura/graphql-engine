import { Database, Feature } from '..';

export const defaultIntrospectionProps = {
  getVersion: async () => Feature.NotImplemented,
  getDriverInfo: async () => Feature.NotImplemented,
  getDatabaseConfiguration: async () => Feature.NotImplemented,
  getDriverCapabilities: async () => Feature.NotImplemented,
  getTrackableTables: async () => Feature.NotImplemented,
  getDatabaseHierarchy: async () => Feature.NotImplemented,
  getTableColumns: async () => Feature.NotImplemented,
  getFKRelationships: async () => Feature.NotImplemented,
  getTablesListAsTree: async () => Feature.NotImplemented,
  getSupportedOperators: async () => Feature.NotImplemented,
  getSupportedScalars: async () => Feature.NotImplemented,
  getTrackableFunctions: async () => Feature.NotImplemented,
  getDatabaseSchemas: async () => Feature.NotImplemented,
  getIsTableView: async () => Feature.NotImplemented,
  getSupportedDataTypes: async () => Feature.NotImplemented,
  getStoredProcedures: async () => Feature.NotImplemented,
  getTrackableObjects: async () => Feature.NotImplemented,
};

export const defaultDatabaseProps: Database = {
  config: {
    getDefaultQueryRoot: async () => Feature.NotImplemented,
    getSupportedQueryTypes: async () => Feature.NotImplemented,
  },
  introspection: defaultIntrospectionProps,
  query: {
    getTableRows: async () => Feature.NotImplemented,
  },
};
