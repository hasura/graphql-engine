import { Database } from '../types/database';
import {
  getTrackableTables,
  getTableColumns,
  getTablesListAsTree,
  getSupportedOperators,
  getTableColumnInfos,
} from './introspection';
import { getTableRows } from './query';
import { isBigQueryTable } from './utils';
import {
  bigquerySupportedFeatures,
  BigQueryTable,
  DataTypeScalars,
  DataTypeToSQLTypeMap,
  bigQueryCapabilities,
} from './types';
import { isFeatureSupported } from '../common/capabilities';
import { parseCreateSQL } from '../common/sqlUtils';
import {
  updateRowsCurry,
  insertRowsCurry,
  deleteRowsCurry,
} from '../common/modify';
import { validateInputRowValues } from '../common/validation';

// createSQLRegex matches one or more sql for creating view, table or functions, and extracts the type, schema, name and also if it is a partition.
// An example string it matches: CREATE TABLE myschema.user(id serial primary key, name text);
// type = table, schema = myschema, nameWithSchema = user, partition = undefined
const createSQLRegex =
  /create\s*(?:|or\s*replace)\s*(?<type>view|table|function)\s*(?:\s*if*\s*not\s*exists\s*)?((?<schema>\"?\w+\"?)\.(?<nameWithSchema>\"?\w+\"?)|(?<name>\"?\w+\"?))\s*(?<partition>partition\s*of)?/gim;

export const bigquery: Database = {
  introspection: {
    getDriverInfo: async () => ({
      name: 'bigquery',
      displayName: 'BigQuery',
      release: 'GA',
    }),
    getDriverCapabilities: async () => {
      return Promise.resolve(bigQueryCapabilities);
    },
    getTrackableTables,
    getDatabaseHierarchy: () => {
      return ['dataset', 'name'];
    },
    getTableColumns,
    getTableColumnInfos,
    getTablesListAsTree,
    getSupportedOperators,
    getSupportedDataTypes: async () => DataTypeToSQLTypeMap,
    getSupportedScalars: async () => DataTypeScalars,
  },
  query: {
    getTableRows,
  },
  modify: {
    insertRows: insertRowsCurry(validateInputRowValues, ''),
    updateRows: updateRowsCurry(validateInputRowValues, ''),
    deleteRows: deleteRowsCurry(''),
  },
  config: {
    getDefaultQueryRoot: ({ table }) => {
      const { name, dataset } = table as BigQueryTable;
      return `${dataset}_${name}`;
    },
    getSupportedQueryTypes: () => {
      return ['select'];
    },
    getViolationActions: () => [],
  },
  check: {
    isFeatureSupported: (feature) =>
      isFeatureSupported(feature, bigquerySupportedFeatures),
    isSchemaModification: () => false,
    isTable: isBigQueryTable,
  },
  utilities: {
    parseCreateSchemaSQL: (sql) =>
      parseCreateSQL(sql, 'bigquery', createSQLRegex),
  },
};
