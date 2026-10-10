import { MetadataTableConfig, SupportedDriver } from '@hasura/shared/types';

export type CustomFieldNamesFormVals = {
  custom_name: string;
  logical_model: string;
} & Required<MetadataTableConfig['custom_root_fields']>;

export type GetTablePayloadArgs = {
  driver: SupportedDriver;
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
