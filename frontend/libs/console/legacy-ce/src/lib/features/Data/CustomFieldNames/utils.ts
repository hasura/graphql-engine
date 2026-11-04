import pickBy from 'lodash/pickBy';
import { MetadataTableConfig, SupportedDriver } from '@hasura/shared/types';
import {
  CustomFieldNamesFormVals,
  GetTablePayloadArgs,
  QualifiedTable,
} from './types';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export const getQualifiedTableForCustomFieldNames = ({
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

export const customFieldNamesPlaceholders = (
  tableName: string,
): CustomFieldNamesFormVals => ({
  custom_name: `${tableName} (default)`,
  select: `${tableName} (default)`,
  select_by_pk: `${tableName}_by_pk (default)`,
  select_aggregate: `${tableName}_aggregate (default)`,
  select_stream: `${tableName}_stream (default)`,
  insert: `insert_${tableName} (default)`,
  insert_one: `insert_${tableName}_one (default)`,
  update: `update_${tableName} (default)`,
  update_by_pk: `update_${tableName}_by_pk (default)`,
  delete: `delete_${tableName} (default)`,
  delete_by_pk: `delete_${tableName}_by_pk (default)`,
  update_many: `update_many_${tableName} (default)`,
  logical_model: 'Select a logical model',
});

export const buildConfigFromFormValues = (
  values: CustomFieldNamesFormVals,
): MetadataTableConfig => {
  // we want to only add properties if a value is "truthy"/not empty

  const config: MetadataTableConfig = {};
  // the shape of the form type almost matches the config type
  const { custom_name, logical_model, ...customRoots } = values;

  if (custom_name) config.custom_name = custom_name;
  if (logical_model) config.logical_model = logical_model;

  const rootsWithValues = pickBy(customRoots, (v) => v !== '');

  if (Object.keys(rootsWithValues).length > 0) {
    config.custom_root_fields = rootsWithValues;
  }

  return config;
};

export const getTrackTableType = (driver: SupportedDriver) => {
  const prefix = getDriverPrefix(driver);
  return `${prefix}_track_table` as const;
};

export const query_field_props: (keyof CustomFieldNamesFormVals)[] = [
  'select',
  'select_by_pk',
  'select_aggregate',
  'select_stream',
];
export const mutation_field_props: (keyof CustomFieldNamesFormVals)[] = [
  'insert',
  'insert_one',
  'update',
  'update_by_pk',
  'delete',
  'delete_by_pk',
];

/**
 * The custom root fields of a table configuration, in the order of the editor
 * (queries, then mutations). The server returns them in no particular order;
 * root fields the editor does not know about come last.
 */
export const getOrderedCustomRootFields = (
  customRootFields: Record<string, string | undefined> = {},
): [string, string][] => {
  const order: string[] = [
    ...query_field_props,
    ...mutation_field_props,
    'update_many',
  ];
  const rank = (key: string) =>
    order.includes(key) ? order.indexOf(key) : order.length;

  return Object.entries(customRootFields)
    .filter((entry): entry is [string, string] => !!entry[1])
    .sort(([a], [b]) => rank(a) - rank(b));
};

export const initFormValues = (currentConfiguration?: MetadataTableConfig) => ({
  custom_name: currentConfiguration?.custom_name || '',
  select: '',
  select_by_pk: '',
  select_aggregate: '',
  select_stream: '',
  insert: '',
  insert_one: '',
  update: '',
  update_by_pk: '',
  delete: '',
  delete_by_pk: '',
  update_many: '',
  logical_model: '',
  ...(currentConfiguration?.custom_root_fields || {}),
});
