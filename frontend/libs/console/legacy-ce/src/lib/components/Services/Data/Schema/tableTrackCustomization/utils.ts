import {
  GetTablePayloadArgs,
  QualifiedTable,
  TrackingTableFormValues,
} from '@/components/Services/Data/Schema/tableTrackCustomization/types';
import { Driver } from '@/dataSources';
import { MetadataTableConfig } from '@/features/MetadataAPI';

export const getTrackingTableFormPlaceholders = (
  tableName: string
): TrackingTableFormValues => {
  return {
    custom_name: `${tableName} (default)`,
    select: `${tableName} (default)`,
    select_by_pk: `${tableName}_by_pk (default)`,
    select_aggregate: `${tableName}_aggregate (default)`,
    select_stream: `${tableName}_stream (default)`,
    insert: `insert_${tableName} (default)`,
    insert_one: `insert_${tableName}_one (default)`,
    update: `update_${tableName} (default)`,
    update_by_pk: `update_by_pk_${tableName} (default)`,
    delete: `delete_${tableName} (default)`,
    delete_by_pk: `delete_by_pk_${tableName} (default)`,
    update_many: `update_many_${tableName} (default)`,
  };
};

export const buildConfigFromFormValues = (
  values: TrackingTableFormValues
): MetadataTableConfig => {
  // we want to only add properties if a value is "truthy"/not empty

  const config: MetadataTableConfig = {};
  // the shape of the form type almost matches the config type
  const { custom_name, ...remainingValues } = values;

  if (custom_name) config.custom_name = custom_name;

  let prop: keyof typeof remainingValues;

  for (prop in remainingValues) {
    if (remainingValues[prop]) {
      if (!config.custom_root_fields) {
        // initialize obj if not yet created
        config.custom_root_fields = {};
      }
      config.custom_root_fields[prop] = remainingValues[prop];
    }
  }

  return config;
};

export const getDriverPrefix = (driver: Driver) =>
  driver === 'postgres' ? 'pg' : driver;

export const getTrackTableType = (driver: Driver) => {
  const prefix = getDriverPrefix(driver);
  return `${prefix}_track_table`;
};

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
