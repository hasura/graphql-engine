import { postgresCapabilities } from '../common/capabilities';
import {
  getDatabaseSchemas,
  getFKRelationships,
  getSupportedOperators,
  getTableColumns,
  getTablesListAsTree,
  getTableColumnInfos,
  getVersion,
  getTrackableTables,
} from './introspection';
import { getDefaultQueryRoot, getTableRows } from './query';
import { DataTypeToSQLTypeMap, DataTypeScalars, isMSSQLTable } from './utils';
import { Database } from '../types/database';
import { mssqlSupportedFeatures } from './types';
import { isFeatureSupported } from '../common/capabilities';
import { parseCreateSQL } from '../common/sqlUtils';
import { createDatabaseSchema, deleteDatabaseSchema } from './modify';
import { validateInputRowValues } from '../common/validation';
import {
  updateRowsCurry,
  insertRowsCurry,
  deleteRowsCurry,
} from '../common/modify';

const createSQLRegex =
  /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>"?\w+"?)\.(?<nameWithSchema>"?\w+"?)|(?<name>"?\w+"?))\s*(?<partition>partition\s*of)?/gim;

export const mssql: Database = {
  introspection: {
    getVersion,
    getDriverInfo: async () => ({
      name: 'mssql',
      displayName: 'MS SQL Server',
      release: 'GA',
    }),
    getDriverCapabilities: async () => Promise.resolve(postgresCapabilities),
    getTrackableTables,
    getDatabaseHierarchy: () => {
      return ['schema', 'name'];
    },
    getTableColumns,
    getTableColumnInfos,
    getFKRelationships,
    getTablesListAsTree,
    getSupportedOperators,
    getDatabaseSchemas,
    getSupportedDataTypes: async () => DataTypeToSQLTypeMap,
    getSupportedScalars: async () => DataTypeScalars,
  },
  modify: {
    createDatabaseSchema,
    deleteDatabaseSchema,
    insertRows: insertRowsCurry(validateInputRowValues, getDefaultQueryRoot),
    updateRows: updateRowsCurry(validateInputRowValues, getDefaultQueryRoot),
    deleteRows: deleteRowsCurry(getDefaultQueryRoot),
  },
  query: {
    getTableRows,
  },
  config: {
    getDefaultQueryRoot,
    getSupportedQueryTypes: () => {
      return ['select', 'insert', 'update', 'delete'];
    },
    getViolationActions: () => [],
  },
  check: {
    isFeatureSupported: (feature) =>
      isFeatureSupported(feature, mssqlSupportedFeatures),
    isSchemaModification: () => false,
    isTable: isMSSQLTable,
  },
  utilities: {
    parseCreateSchemaSQL: (sql) => parseCreateSQL(sql, 'mssql', createSQLRegex),
  },
};
