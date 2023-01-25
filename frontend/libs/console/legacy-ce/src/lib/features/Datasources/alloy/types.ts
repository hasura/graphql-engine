import { DataTarget } from '../drivers';

export interface AlloyDataTarget {
  database: string;
  schema: string;
  table: string;
  kind?: 'postgres';
}

export const isAlloyDataTarget = (
  target: DataTarget
): target is AlloyDataTarget => {
  return 'database' in target && 'schema' in target && 'table' in target;
};
