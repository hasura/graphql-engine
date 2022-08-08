import { Database } from '..';
import { getDatabaseConfiguration, getTrackableTables } from './introspection';

export const postgres: Database = {
  introspection: {
    getDatabaseConfiguration,
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['schema', 'name'];
    },
  },
};
