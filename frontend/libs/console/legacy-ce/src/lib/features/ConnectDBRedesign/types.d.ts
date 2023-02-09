export type DatabaseKind =
  | 'postgres'
  | 'mssql'
  | 'bigquery'
  | 'citus'
  | 'alloydb'
  | 'snowflake'
  | 'athena'
  | 'cockroach';

export type EEState = 'active' | 'inactive' | 'expired';
