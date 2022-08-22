import { Database } from '..';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
  getFKRelationships,
} from './introspection';

export type PostgresTable = { name: string; schema: string };

export const postgres: Database = {
  introspection: {
    getDatabaseConfiguration,
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getFKRelationships,
  },
};
