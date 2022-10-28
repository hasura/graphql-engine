import { Database, Feature } from '..';

export type MySQLTable = { name: string };

export const postgres: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'mysql',
      displayName: 'MySQL',
      release: 'disabled',
    }),
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
