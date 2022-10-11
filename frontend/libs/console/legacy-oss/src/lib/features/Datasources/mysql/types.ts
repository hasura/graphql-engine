import { DataTarget } from '../drivers';

export const isMysqlDataTarget = (
  target: DataTarget
): target is MysqlDataTarget => {
  return 'database' in target && 'table' in target;
};

export interface MysqlDataTarget {
  database: string;
  table: string;
  kind?: 'mysql';
}
