import { generateTableDef } from './common';

export interface DataSourcesAPI {
  getEstimateCountQuery(tableName: string, schemaName: string): string;
  fetchColumnDefaultFunctionsQuery(schemaName: string): string;
  fetchColumnTypesQuery: string;
}

export type Driver = 'postgresql'; // | 'mysql';

export type DataAPI = {
  [key in keyof DataSourcesAPI]: (driver: Driver) => DataSourcesAPI[key];
} & {
  generateTableDef: typeof generateTableDef;
};
