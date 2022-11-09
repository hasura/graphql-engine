import { DataTarget } from '../drivers';

export const isPostgresDataTarget = (
  target: DataTarget
): target is PostgresDataTarget => {
  return 'database' in target && 'schema' in target && 'table' in target;
};

export interface PostgresDataTarget {
  database: string;
  schema: string;
  table: string;
  kind?: 'postgres';
}
