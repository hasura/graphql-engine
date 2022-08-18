import { Database } from '..';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
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
  },
};
