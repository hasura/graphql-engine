import { MetadataTableConfig } from '../../hasura-metadata-types';
import { Driver } from '../../../dataSources';

export type CustomFieldNamesFormVals = {
  custom_name: string;
  logical_model: string;
} & Required<MetadataTableConfig['custom_root_fields']>;

type GetTablePayloadArgs = {
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
