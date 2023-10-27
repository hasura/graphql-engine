import { TableColumn } from '../../../../../../DataSource';
import { Metadata } from '../../../../../../hasura-metadata-types';
import { isPermission } from '../../../../../utils';
import {
  MetadataDataSource,
  TableEntry,
} from '../../../../../../../metadata/types';
import z from 'zod';
import { inputValidationSchema } from '../../../../../../../components/Services/Data/TablePermissions/InputValidation/InputValidation';

type Operation = 'insert' | 'select' | 'update' | 'delete';

const supportedQueries: Operation[] = ['select'];

export const getAllowedFilterKeys = (
  query: 'insert' | 'select' | 'update' | 'delete'
): ('check' | 'filter')[] => {
  switch (query) {
    case 'insert':
      return ['check'];
    case 'update':
      return ['filter', 'check'];
    default:
      return ['filter'];
  }
};

type GetMetadataTableArgs = {
  dataSourceName: string;
  table: unknown;
  trackedTables: TableEntry[] | undefined;
};

const getMetadataTable = (args: GetMetadataTableArgs) => {
  const { table, trackedTables } = args;

  const selectedTable = trackedTables?.find(
    trackedTable => JSON.stringify(trackedTable.table) === JSON.stringify(table)
  );

  // find selected table
  return {
    table: selectedTable,
    tables: trackedTables,
    // for gdc tables will be an array of strings so this needs updating
    tableNames: trackedTables?.map(each => each.table),
  };
};

const getRoles = (metadataTables?: TableEntry[]) => {
  // go through all tracked tables
  const res = metadataTables?.reduce<Set<string>>((acc, each) => {
    // go through all permissions
    Object.entries(each).forEach(([key, value]) => {
      const props = { key, value };
      // check object key of metadata is a permission
      if (isPermission(props)) {
        // add each role from each permission to the set
        props.value.forEach(permission => {
          acc.add(permission.role);
        });
      }
    });

    return acc;
  }, new Set());
  return Array.from(res || []);
};

export interface CreateFormDataArgs {
  dataSourceName: string;
  table: unknown;
  metadata: Metadata;
  tableColumns: TableColumn[];
  metadataSource: MetadataDataSource;
  trackedTables: TableEntry[];
  validateInput: z.infer<typeof inputValidationSchema>;
}

export const createFormData = (props: CreateFormDataArgs) => {
  const { dataSourceName, table, tableColumns, trackedTables } = props;
  // find the specific metadata table
  const metadataTable = getMetadataTable({
    dataSourceName,
    table,
    trackedTables: trackedTables,
  });

  const roles = getRoles(metadataTable.tables);

  return {
    roles,
    supportedQueries,
    tableNames: metadataTable.tableNames,
    columns: tableColumns?.map(({ name }) => name),
  };
};
