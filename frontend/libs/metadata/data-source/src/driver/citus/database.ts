import { Database } from '../types/database';
import { checkSchemaModification } from '../common/utils';
import {
  getTableColumns,
  getSupportedOperators,
  getTableColumnInfos,
  getTrackableTables,
} from './introspection';
import { getTableRows } from './query';
import { postgresCapabilities } from '../common/capabilities';
import {
  isPostgresTable,
  parsePostgresCreateSchemaSQL,
} from '../postgres/utils';
import { citusSupportedFeatures } from './types';
import { isFeatureSupported } from '../common/capabilities';
import {
  getPostgresIntrospectionMethods,
  getTablesListAsTree,
} from '../postgres/introspection';
import { createPostgresModifyMethods } from '../postgres/modify';
import { postgres } from '../postgres/database';
import { consoleDataTypeToSQLTypeMap, consoleScalars } from '../postgres';

export const citus: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'citus',
      displayName: 'Citus',
      release: 'GA',
    }),
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
    getTrackableTables,
    getDatabaseHierarchy: () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getTableColumnInfos,
    getTablesListAsTree,
    getSupportedOperators,
    getSupportedDataTypes: async () => consoleDataTypeToSQLTypeMap,
    getSupportedScalars: async () => consoleScalars,
    ...getPostgresIntrospectionMethods('citus'),
  },
  modify: createPostgresModifyMethods('citus'),
  query: {
    getTableRows,
  },
  config: postgres.config,
  check: {
    isFeatureSupported: (feature) =>
      isFeatureSupported(feature, citusSupportedFeatures),
    isSchemaModification: checkSchemaModification,
    isTable: isPostgresTable,
  },
  utilities: {
    parseCreateSchemaSQL: parsePostgresCreateSchemaSQL,
  },
};
