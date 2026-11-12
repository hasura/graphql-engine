import {
  getTableColumnInfosCurry,
  getTableColumnsCurry,
} from '../../postgres/introspection/getTableColumns';

export const getTableColumns = getTableColumnsCurry('citus');
export const getTableColumnInfos = getTableColumnInfosCurry('citus');
export { getSupportedOperators } from './getSupportedOperators';
export * from './getTrackableTables';
