import {
  ChangeDatabaseSchemaProps,
  Database,
  GetDefaultQueryRootProps,
  GetVersionProps,
} from '..';
import { Table } from '../../hasura-metadata-types';
import { runSQL } from '../api';
import { postgresCapabilities } from '../common/capabilities';
import {
  defaultDatabaseProps,
  defaultIntrospectionProps,
} from '../common/defaultDatabaseProps';
import {
  getDatabaseConfiguration,
  getDatabaseSchemas,
  getFKRelationships,
  getSupportedOperators,
  getTrackableFunctions,
  getTableColumns,
  getTablesListAsTree,
  getTrackableTables,
  getIsTableView,
} from './introspection';
import { getTableRows } from './query';
import { consoleDataTypeToSQLTypeMap, consoleScalars } from './utils';

export type PostgresTable = { name: string; schema: string };

const getDropSchemaSql = (schemaName: string) =>
  `drop schema "${schemaName}" cascade;`;

const getCreateSchemaSql = (schemaName: string) =>
  `create schema "${schemaName}";`;

export const postgres: Database = {
  ...defaultDatabaseProps,
  introspection: {
    ...defaultIntrospectionProps,
    getTrackableFunctions,
    getVersion: async ({ dataSourceName, httpClient }: GetVersionProps) => {
      const result = await runSQL({
        source: {
          name: dataSourceName,
          kind: 'postgres',
        },
        sql: `SELECT VERSION()`,
        httpClient,
      });
      return result.result?.[1][0] ?? '';
    },
    getDriverInfo: async () => ({
      name: 'postgres',
      displayName: 'Postgres',
      release: 'GA',
      native: true,
    }),
    getDatabaseConfiguration,
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
    getTrackableTables,
    getDatabaseHierarchy: async () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getFKRelationships,
    getTablesListAsTree,
    getSupportedOperators,
    getDatabaseSchemas,
    getIsTableView,
    getSupportedDataTypes: async () => consoleDataTypeToSQLTypeMap,
    getSupportedScalars: async () => consoleScalars,
  },
  query: {
    getTableRows,
  },
  modify: {
    defaultQueryRoot: async ({ table }: GetDefaultQueryRootProps) => {
      const { name, schema } = table as PostgresTable;

      return schema === 'public' ? name : `${schema}_${name}`;
    },
    createDatabaseSchema: async ({
      dataSourceName,
      schemaName,
      httpClient,
    }: ChangeDatabaseSchemaProps) => {
      const response = await runSQL({
        source: { name: dataSourceName, kind: 'postgres' },
        sql: getCreateSchemaSql(schemaName),
        httpClient,
      });
      return response;
    },
    deleteDatabaseSchema: async ({
      dataSourceName,
      schemaName,
      httpClient,
    }: ChangeDatabaseSchemaProps) => {
      const response = await runSQL({
        source: { name: dataSourceName, kind: 'postgres' },
        sql: getDropSchemaSql(schemaName),
        httpClient,
      });
      return response;
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
