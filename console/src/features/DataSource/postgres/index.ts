import { Database } from '..';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
  getFKRelationships,
  getTablesListAsTree,
} from './introspection';

export type PostgresTable = { name: string; schema: string };

export const postgres: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'postgres',
      displayName: 'Postgres',
      release: 'GA',
    }),
    getDatabaseConfiguration,
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getFKRelationships,
    getTablesListAsTree,
  },
};
