import { PostgresTable } from '../postgresql/types';

export interface CitusTable extends PostgresTable {
  citus_table_type: string;
}
