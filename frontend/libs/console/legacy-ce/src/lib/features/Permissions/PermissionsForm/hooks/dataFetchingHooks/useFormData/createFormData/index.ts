import { TableColumn } from '@hasura/metadata/data-source';
import {
  ComputedField,
  MetadataTable,
  Source,
  Table,
} from '@hasura/shared/types';
import { isPermission } from '../../../../../utils';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { TablePermissionInputValidationSchema } from '../../../../components/InputValidation/InputValidation';

type Operation = 'insert' | 'select' | 'update' | 'delete';

const supportedQueries: Operation[] = ['select'];

export const getAllowedFilterKeys = (
  query: 'insert' | 'select' | 'update' | 'delete',
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
  trackedTables: MetadataTable[] | undefined;
};

const getMetadataTable = (args: GetMetadataTableArgs) => {
  const { table, trackedTables } = args;

  const selectedTable = trackedTables?.find((trackedTable) =>
    areTablesEqual(trackedTable.table, table as Table),
  );

  // find selected table
  return {
    table: selectedTable,
    tables: trackedTables,
    // for gdc tables will be an array of strings so this needs updating
    tableNames: trackedTables?.map((each) => each.table),
  };
};

const getRoles = (metadataTables?: MetadataTable[]) => {
  // go through all tracked tables
  const res = metadataTables?.reduce<Set<string>>((acc, each) => {
    // go through all permissions
    Object.entries(each).forEach(([key, value]) => {
      const props = { key, value };
      // check object key of metadata is a permission
      if (isPermission(props)) {
        // add each role from each permission to the set
        props.value.forEach((permission) => {
          acc.add(permission.role);
        });
      }
    });

    return acc;
  }, new Set());
  return Array.from(res || []);
};

export interface CreateFormDataArgs {
  table: unknown;
  tableColumns: TableColumn[];
  source: Source;
  validateInput: TablePermissionInputValidationSchema;
  computedFields: ComputedField[];
}

export const createFormData = (props: CreateFormDataArgs) => {
  const { source, table, tableColumns, computedFields } = props;
  // find the specific metadata table
  const metadataTable = getMetadataTable({
    dataSourceName: source.name,
    table,
    trackedTables: source.tables,
  });

  const roles = getRoles(metadataTable.tables);

  return {
    roles,
    supportedQueries,
    tableNames: metadataTable.tableNames,
    columns: tableColumns?.map(({ name }) => name),
    computed_fields: computedFields.map(({ name }) => name),
  };
};
