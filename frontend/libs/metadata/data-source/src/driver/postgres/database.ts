import {
  postgresCapabilities,
  postgresSupportedFeatures,
} from '../common/capabilities';
import {
  getDatabaseSchemas,
  getFKRelationships,
  getSupportedOperators,
  getTrackableFunctions,
  getTableColumns,
  getTablesListAsTree,
  getTrackableTables,
  getTableColumnInfos,
  getVersion,
  getPostgresIntrospectionMethods,
} from './introspection';
import { getTableRows } from './query';
import { isPostgresTable, parsePostgresCreateSchemaSQL } from './utils';
import { Database } from '../types/database';
import { consoleDataTypeToSQLTypeMap, consoleScalars } from './types';
import { isFeatureSupported } from '../common/capabilities';
import { checkSchemaModification } from '../common/utils';
import { createPostgresModifyMethods } from './modify';
import {
  getDefaultQueryRoot,
  getFrequentlyUsedColumns,
  getViolationActions,
} from './config';
import { getPostgresFunctionDefinition } from './introspection/getFunctionDefinition';
import {
  getFunctionComment,
  getTableComment,
  getViewComment,
} from './introspection/getTableComment';
import { getPostgresViewDefinition } from './introspection/getViewDefinition';
import { statementTimeoutSQL } from './sqlQueries';

export const postgres: Database = {
  introspection: {
    // Shared Postgres-family methods (keys, check constraints, indexes,
    // triggers, ...); the entries below override them where they overlap.
    ...getPostgresIntrospectionMethods('postgres'),
    getTrackableFunctions,
    getVersion,
    getDriverInfo: async () => ({
      name: 'postgres',
      displayName: 'Postgres',
      release: 'GA',
      native: true,
    }),
    // getDatabaseConfiguration,
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
    getTrackableTables,
    getDatabaseHierarchy: () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getTableColumnInfos,
    getTableComment,
    getViewComment,
    getFunctionComment,
    getFKRelationships,
    getTablesListAsTree,
    getSupportedOperators,
    getDatabaseSchemas,
    getSupportedDataTypes: async () => consoleDataTypeToSQLTypeMap,
    getSupportedScalars: async () => consoleScalars,
    getFunctionDefinition: getPostgresFunctionDefinition,
    getViewDefinition: getPostgresViewDefinition,
  },
  query: {
    getTableRows,
  },
  modify: createPostgresModifyMethods('postgres'),
  config: {
    getDefaultQueryRoot,
    getSupportedQueryTypes: () => {
      return ['select', 'insert', 'update', 'delete'];
    },
    getViolationActions,
    getFrequentlyUsedColumns,
  },
  check: {
    isFeatureSupported: (feature) =>
      isFeatureSupported(feature, postgresSupportedFeatures),
    isSchemaModification: checkSchemaModification,
    isTable: isPostgresTable,
  },
  utilities: {
    parseCreateSchemaSQL: parsePostgresCreateSchemaSQL,
    statementTimeoutSQL,
  },
};
