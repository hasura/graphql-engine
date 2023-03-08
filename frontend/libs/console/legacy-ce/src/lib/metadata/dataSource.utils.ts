import { NativeDrivers } from '../features/hasura-metadata-types';

export type DataSourceDriver =
  | 'postgres'
  | 'mysql'
  | 'mssql'
  | 'bigquery'
  | 'citus'
  | 'cockroach'
  | 'alloy';

export const getDataSourcePrefix = (driver: DataSourceDriver) => {
  if (driver === 'postgres') {
    return 'pg_';
  }

  return `${driver}_`;
};

export const isPostgres = (driverName: NativeDrivers) =>
  driverName === 'postgres' || driverName === 'alloy';
