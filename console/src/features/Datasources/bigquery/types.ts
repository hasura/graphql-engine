import { DataTarget } from '../drivers';

export const isBigQueryDataTarget = (
  target: DataTarget
): target is BigQueryDataTarget => {
  return 'database' in target && 'dataset' in target && 'table' in target;
};
export interface BigQueryDataTarget {
  database: string;
  dataset: string;
  table: string;
  kind?: 'bigquery';
}
