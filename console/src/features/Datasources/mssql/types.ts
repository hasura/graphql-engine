import { DataTarget } from '../drivers';

export const isMSSQLDataTarget = (
  target: DataTarget
): target is MssqlDataTarget => {
  return 'database' in target && 'schema' in target && 'table' in target;
};

export interface MssqlDataTarget {
  database: string;
  schema: string;
  table: string;
  kind?: 'mssql';
}
