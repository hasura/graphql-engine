import { Table } from '@/features/hasura-metadata-types';
import { Database, GetDefaultQueryRootProps } from '..';
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
  modify: {
    defaultQueryRoot: async ({ table }: GetDefaultQueryRootProps) => {
      const { name, schema } = table as PostgresTable;

      return schema === 'public' ? name : `${schema}_${name}`;
    },
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, schema } = table as PostgresTable;
      return schema === 'public' ? name : `${schema}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select', 'insert', 'update', 'delete'];
    },
  },
};
