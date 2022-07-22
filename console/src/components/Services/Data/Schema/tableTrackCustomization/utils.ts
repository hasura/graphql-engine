import { Driver } from '@/dataSources';

type TrackingTableFormPlaceholders = {
  custom_name: string;
  select: string;
  select_by_pk: string;
  select_aggregate: string;
  insert: string;
  insert_one: string;
  update: string;
  update_by_pk: string;
  delete: string;
  delete_by_pk: string;
};

export const getTrackingTableFormPlaceholders = (
  tableName: string
): TrackingTableFormPlaceholders => {
  return {
    custom_name: `${tableName} (default)`,
    select: `${tableName} (default)`,
    select_by_pk: `${tableName}_by_pk (default)`,
    select_aggregate: `${tableName}_aggregate (default)`,
    insert: `insert_${tableName} (default)`,
    insert_one: `insert_${tableName}_one (default)`,
    update: `update_${tableName} (default)`,
    update_by_pk: `update_by_pk_${tableName} (default)`,
    delete: `delete_${tableName} (default)`,
    delete_by_pk: `delete_by_pk_${tableName} (default)`,
  };
};

export const getDriverPrefix = (driver: Driver) =>
  driver === 'postgres' ? 'pg' : driver;

export const getTrackTableType = (driver: Driver) => {
  const prefix = getDriverPrefix(driver);
  return `${prefix}_track_table`;
};

export type GetTablePayloadArgs = {
  driver: Driver;
  schema: string;
  tableName: string;
};

type BigQueryQualifiedTable = {
  dataset: string;
};

type SchemaQualifiedTable = {
  schema: string;
};

export type QualifiedTable = {
  name: string;
} & (BigQueryQualifiedTable | SchemaQualifiedTable);

export const getQualifiedTable = ({
  driver,
  schema,
  tableName,
}: GetTablePayloadArgs): QualifiedTable => {
  if (driver === 'bigquery') {
    return {
      dataset: schema,
      name: tableName,
    };
  }
  return {
    schema,
    name: tableName,
  };
};
