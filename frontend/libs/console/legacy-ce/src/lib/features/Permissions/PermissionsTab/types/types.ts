import { Driver } from '../../../../dataSources';

/**
 * - contains `dataSource`: driver (e.g. postgres, mssql, etc.) and database name (e.g. default, test, etc.)
 * - contains dataLeaf: a recursive data structure that contains names of schemas and tables
 * - e.g. for postgres dataLeaf would be something like:
 *  {
 *    type: 'schema',
 *    name: 'public',
 *    leaf: {
 *      type: 'table',
 *      name: 'users'
 *    }
 *  }
 */
export interface NewDataTarget {
  dataSource: NewDataSource;
  dataLeaf: DataLeaf;
}

export interface NewDataSource {
  driver: Driver;
  database: string;
}

/**
 * - a recursive data structure that contains names of schemas and tables
 * - e.g. for postgres dataLeaf would be something like:
 *  {
 *    type: 'schema',
 *    name: 'public',
 *    leaf: {
 *      type: 'table',
 *      name: 'users'
 *    }
 *  }
 */
export interface DataLeaf {
  type: string;
  name: string;
  leaf?: DataLeaf;
}

export type SupportedDrivers =
  | 'postgres'
  | 'bigquery'
  | 'mssql'
  | 'citus'
  | 'gdc';
