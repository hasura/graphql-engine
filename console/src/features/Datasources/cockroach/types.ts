import { DataTarget } from '../drivers';

export interface CockroachDataTarget {
  database: string;
  schema: string;
  table: string;
  kind?: 'cockroach';
}

export const isCockroachDataTarget = (
  target: DataTarget
): target is CockroachDataTarget => {
  return 'database' in target && 'schema' in target && 'table' in target;
};
