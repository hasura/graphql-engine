import { DataTarget } from '../drivers';

export const isCitusDataTarget = (
  target: DataTarget
): target is CitusDataTarget => {
  return 'database' in target && 'schema' in target && 'table' in target;
};

export interface CitusDataTarget {
  database: string;
  schema: string;
  table: string;
  kind?: 'citus';
}
