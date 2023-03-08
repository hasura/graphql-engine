import { Table } from '../../hasura-metadata-types';
import { Database } from '..';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
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
  ...defaultDatabaseProps,
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
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, schema } = table as AlloyDbTable;
      return schema === 'public' ? name : `${schema}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select', 'insert', 'update', 'delete'];
    },
  },
};
