import { isArray, isObject } from '../../components/Common/utils/jsUtils';
import { Table } from '../hasura-metadata-types';
import { GDCTable } from './gdc';
import { TableColumn } from './types';

export type SchemaTable = {
  name: string;
  schema: string;
};

export type DatasetTable = {
  name: string;
  dataset: string;
};

export const isSchemaTable = (table: Table): table is SchemaTable =>
  isObject(table) && 'schema' in table && 'name' in table;

export const isDatasetTable = (table: Table): table is DatasetTable =>
  isObject(table) && 'dataset' in table && 'name' in table;

export const isGDCTable = (table: Table): table is GDCTable => isArray(table);

export function columnDataType(dataType: TableColumn['dataType']): string {
  if (typeof dataType === 'string') {
    return dataType;
  }
  return dataType.type;
}
