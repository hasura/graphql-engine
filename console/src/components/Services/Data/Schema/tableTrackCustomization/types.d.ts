import { MetadataTableConfig } from '@/features/MetadataAPI';
import { Driver } from '@/dataSources';

export type TrackingTableFormValues = {
  custom_name: string;
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
