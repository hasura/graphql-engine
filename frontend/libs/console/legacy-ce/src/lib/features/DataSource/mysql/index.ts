import { Database, Feature } from '..';
import {
  defaultDatabaseProps,
  defaultIntrospectionProps,
} from '../common/defaultDatabaseProps';

export type MySQLTable = { name: string };

export const mysql: Database = {
  ...defaultDatabaseProps,
  introspection: {
    ...defaultIntrospectionProps,
    getDriverInfo: async () => ({
      name: 'mysql',
      displayName: 'MySQL',
      release: 'disabled',
    }),
    getDatabaseConfiguration: async () => Feature.NotImplemented,
    getDriverCapabilities: async () => Feature.NotImplemented,
    getTrackableTables: async () => Feature.NotImplemented,
    getDatabaseHierarchy: async () => Feature.NotImplemented,
    getTableColumns: async () => Feature.NotImplemented,
    getFKRelationships: async () => Feature.NotImplemented,
    getTablesListAsTree: async () => Feature.NotImplemented,
    getSupportedOperators: async () => Feature.NotImplemented,
    getDatabaseSchemas: async () => Feature.NotImplemented,
    getIsTableView: async () => Feature.NotImplemented,
  },
  query: {
    getTableRows: async () => Feature.NotImplemented,
  },
  config: {
    getDefaultQueryRoot: async () => Feature.NotImplemented,
    getSupportedQueryTypes: async () => Feature.NotImplemented,
  },
};
