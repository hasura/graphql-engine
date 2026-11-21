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

type PlaceholderOptions = {
  /** the source's naming convention, `hasura-default` when not set */
  namingConvention?: string;
  /** whether `tableName` is a custom name, which is never re-cased */
  isCustomName?: boolean;
};

const upperFirst = (s: string) => s.charAt(0).toUpperCase() + s.slice(1);
const lowerFirst = (s: string) => s.charAt(0).toLowerCase() + s.slice(1);

// like the server, split a generated name on `_`, but keep a leading `_` as part
// of the first word
const splitSnake = (name: string) => {
  const parts = name.split('_');
  const words =
    parts[0] === '' && parts.length > 1
      ? [`_${parts[1]}`, ...parts.slice(2)]
      : parts;
  return words.filter((w) => w !== '');
};

/**
 * Builds the root field name the server generates for a table, e.g.
 * `update_author_by_pk` or, with the `graphql-default` naming convention,
 * `updateAuthorByPk`.
 */
const rootFieldName = (
  tableName: string,
  prefix: string[],
  suffix: string[],
  { namingConvention, isCustomName }: PlaceholderOptions,
) => {
  if (namingConvention !== 'graphql-default') {
    return [...prefix, tableName, ...suffix].join('_');
  }

  // the first word is camel-cased unless it is a custom name, every other word
  // starts with an upper case letter
  const words: { text: string; custom: boolean }[] = [
    ...prefix.map((text) => ({ text, custom: false })),
    ...(isCustomName
      ? [{ text: tableName, custom: true }]
      : splitSnake(tableName).map((text) => ({ text, custom: false }))),
    ...suffix.map((text) => ({ text, custom: false })),
  ];

  return words
    .map(({ text, custom }, i) => {
      if (i > 0) return upperFirst(text);
      return custom ? text : lowerFirst(text);
    })
    .join('');
};

export const customFieldNamesPlaceholders = (
  tableName: string,
  options: PlaceholderOptions = {},
): CustomFieldNamesFormVals => {
  const name = (prefix: string[], suffix: string[] = []) =>
    `${rootFieldName(tableName, prefix, suffix, options)} (default)`;

  return {
    custom_name: name([]),
    select: name([]),
    select_by_pk: name([], ['by', 'pk']),
    select_aggregate: name([], ['aggregate']),
    select_stream: name([], ['stream']),
    insert: name(['insert']),
    insert_one: name(['insert'], ['one']),
    update: name(['update']),
    update_by_pk: name(['update'], ['by', 'pk']),
    delete: name(['delete']),
    delete_by_pk: name(['delete'], ['by', 'pk']),
    update_many: name(['update'], ['many']),
    logical_model: 'Select a logical model',
  };
};

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
