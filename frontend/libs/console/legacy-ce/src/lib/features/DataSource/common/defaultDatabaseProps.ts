import { Database, Feature } from '..';

export const defaultDatabaseProps: Database = {
  config: {
    getDefaultQueryRoot: async () => Feature.NotImplemented,
    getSupportedQueryTypes: async () => Feature.NotImplemented,
  },
  introspection: {
    getDriverInfo: async () => Feature.NotImplemented,
    getDatabaseConfiguration: async () => Feature.NotImplemented,
    getTrackableTables: async () => Feature.NotImplemented,
    getDatabaseHierarchy: async () => Feature.NotImplemented,
    getTableColumns: async () => Feature.NotImplemented,
    getFKRelationships: async () => Feature.NotImplemented,
    getTablesListAsTree: async () => Feature.NotImplemented,
    getSupportedOperators: async () => Feature.NotImplemented,
  },
  query: {
    getTableRows: async () => Feature.NotImplemented,
  },
};
