import { Table } from '../../hasura-metadata-types';
import {
  defaultDatabaseProps,
  defaultIntrospectionProps,
} from '../common/defaultDatabaseProps';
import { Database, Feature } from '../index';
import {
  getTablesListAsTree,
  getTrackableTables,
  getTableColumns,
  getDatabaseConfiguration,
  getSupportedOperators,
  getFKRelationships,
  getDriverCapabilities,
  getTrackableObjects,
  getSupportedScalars,
} from './introspection';
import { getTableRows } from './query';

/**
 * Why is GDCTable as string[] ?
 * It denotes the table along with it's hierarchy based on the DB. For example, in a mysql source
 * you'd have just the table name -> ["Album"] but in a db with schemas -> ["Public", "Album"].
 */
export type GDCTable = string[];
export type GDCFunction = string[];

export const gdc: Database = {
  ...defaultDatabaseProps,
  introspection: {
    ...defaultIntrospectionProps,
    getDriverInfo: async () => Feature.NotImplemented,
    getDatabaseConfiguration,
    getDriverCapabilities,
    getTrackableTables,
    getTrackableObjects,
    getDatabaseHierarchy: async () => Feature.NotImplemented,
    getTableColumns,
    getFKRelationships,
    getTablesListAsTree,
    getSupportedOperators,
    getSupportedScalars,
    getDatabaseSchemas: async () => Feature.NotImplemented,
    getIsTableView: async () => Feature.NotImplemented,
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot: async (table: Table) => {
      return (table as GDCTable).join('_');
    },
    getSupportedQueryTypes: async () => {
      return ['select', 'delete', 'update', 'insert'];
    },
  },
};
