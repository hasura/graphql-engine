import { Database } from '../index';
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
  getTableColumnInfos,
} from './introspection';
import { getTableRows } from './query';
import { getDefaultQueryRoot } from './config';
import { gdcSupportedFeatures } from './types';
import { isFeatureSupported } from '../common/capabilities';
import { validateInputRowValues } from '../common/validation';
import { getSupportedDataTypes } from './introspection/getSupportedDataTypes';
import {
  updateRowsCurry,
  insertRowsCurry,
  deleteRowsCurry,
} from '../common/modify';

export const gdc: Database = {
  introspection: {
    getDatabaseConfiguration,
    getDriverCapabilities,
    getTrackableTables,
    getTrackableObjects,
    getTableColumns,
    getFKRelationships,
    getTablesListAsTree,
    getSupportedOperators,
    getSupportedScalars,
    getTableColumnInfos,
    getSupportedDataTypes,
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
    getDefaultQueryRoot,
    getSupportedQueryTypes: () => {
      return ['select', 'delete', 'update', 'insert'];
    },
    getViolationActions: () => [],
  },
  check: {
    isFeatureSupported: (feature) =>
      isFeatureSupported(feature, gdcSupportedFeatures),
    isSchemaModification: () => false,
    isTable: () => true,
  },
  utilities: {},
};
