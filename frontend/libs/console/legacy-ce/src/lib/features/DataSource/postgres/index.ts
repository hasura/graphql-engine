import { Table } from '@/features/hasura-metadata-types';
import { Database } from '..';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
import {
  getDatabaseConfiguration,
  getTrackableTables,
  getTableColumns,
  getFKRelationships,
  getTablesListAsTree,
  getSupportedOperators,
} from './introspection';
import { getTableRows } from './query';

export type PostgresTable = { name: string; schema: string };

export const postgres: Database = {
  ...defaultDatabaseProps,
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
    getSupportedOperators,
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, schema } = table as PostgresTable;
      return schema === 'public' ? name : `${schema}_${name}`;
    },
  },
};
