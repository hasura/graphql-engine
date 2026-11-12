import {
  getTableColumnInfosCurry,
  getTableColumnsCurry,
} from '../../postgres/introspection/getTableColumns';

export const getTableColumns = getTableColumnsCurry('cockroach');
export const getTableColumnInfos = getTableColumnInfosCurry('cockroach');
export { getSupportedOperators } from './getSupportedOperators';
export * from './getTrackableTables';
