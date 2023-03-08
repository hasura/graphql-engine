import { Table } from '../../hasura-metadata-types';
import { Database, Feature } from '..';
import { defaultDatabaseProps } from '../common/defaultDatabaseProps';
import {
  getTrackableTables,
  getTableColumns,
  getTablesListAsTree,
  getSupportedOperators,
} from './introspection';
import { getTableRows } from './query';

export type BigQueryTable = { name: string; dataset: string };

export const bigquery: Database = {
  ...defaultDatabaseProps,
  introspection: {
    getDriverInfo: async () => ({
      name: 'bigquery',
      displayName: 'BigQuery',
      release: 'GA',
    }),
    getDatabaseConfiguration: async () => {
      return Feature.NotImplemented;
    },
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['dataset', 'name'];
    },
    getTableColumns,
    getFKRelationships: async () => Feature.NotImplemented,
    getTablesListAsTree,
    getSupportedOperators,
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      const { name, dataset } = table as BigQueryTable;
      return `${dataset}_${name}`;
    },
    getSupportedQueryTypes: async () => {
      return ['select'];
    },
  },
};
