import { Database } from '../types/database';
import { checkSchemaModification } from '../common/utils';
import {
  getTableColumns,
  getSupportedOperators,
  getTrackableTables,
} from './introspection';
import { getTableRows } from './query';
import { postgresCapabilities } from '../common/capabilities';
import { consoleDataTypeToSQLTypeMap, consoleScalars } from '../postgres/types';
import { cockroachSupportedFeatures } from './types';
import { isFeatureSupported } from '../common/capabilities';
import {
  getPostgresIntrospectionMethods,
  getTablesListAsTree,
} from '../postgres/introspection';
import {
  isPostgresTable,
  parsePostgresCreateSchemaSQL,
} from '../postgres/utils';
import { createPostgresModifyMethods } from '../postgres/modify';
import { postgres } from '../postgres/database';
import { getFrequentlyUsedColumns } from './config';

export const cockroach: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'cockroach',
      displayName: 'CockroachDB',
      release: 'GA',
    }),
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
    getTrackableTables,
    getDatabaseHierarchy: () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getTableColumnInfos: getTableColumns,
    getTablesListAsTree,
    getSupportedOperators,
    getSupportedDataTypes: async () => consoleDataTypeToSQLTypeMap,
    getSupportedScalars: async () => consoleScalars,
    ...getPostgresIntrospectionMethods('cockroach'),
  },
  query: {
    getTableRows,
  },
  config: {
    ...postgres.config,
    getFrequentlyUsedColumns,
  },
  modify: createPostgresModifyMethods('cockroach'),
  check: {
    isFeatureSupported: (feature) =>
      isFeatureSupported(feature, cockroachSupportedFeatures),
    isSchemaModification: checkSchemaModification,
    isTable: isPostgresTable,
  },
  utilities: {
    parseCreateSchemaSQL: parsePostgresCreateSchemaSQL,
  },
};
