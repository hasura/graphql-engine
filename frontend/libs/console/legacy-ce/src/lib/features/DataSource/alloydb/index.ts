import { Database } from '..';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
  getFKRelationships,
  getTablesListAsTree,
  getSupportedOperators,
} from '../postgres/introspection';
import { getTableRows } from '../postgres/query';

export type AlloyDbTable = { name: string; schema: string };

export const alloy: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'alloy',
      displayName: 'AlloyDB',
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
    getSupportedOperators,
  },
  query: {
    getTableRows,
  },
};
