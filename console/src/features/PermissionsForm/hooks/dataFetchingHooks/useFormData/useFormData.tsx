import { useQuery } from 'react-query';

import { DataSource, exportMetadata, TableColumn } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';

import { Metadata, MetadataTable } from '@/features/MetadataAPI';
import { isPermission } from '../utils';

type Operation = 'insert' | 'select' | 'update' | 'delete';

const supportedQueries: Operation[] = ['insert', 'select', 'update', 'delete'];

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
  metadata: Metadata;
};

const getMetadataTable = (args: GetMetadataTableArgs) => {
  const { dataSourceName, table, metadata } = args;

  const trackedTables = metadata.metadata?.sources?.find(
    source => source.name === dataSourceName
  )?.tables;

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

const getRoles = (metadataTables?: MetadataTable[]) => {
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

interface CreateFormDataArgs {
  dataSourceName: string;
  table: unknown;
  metadata: Metadata;
  tableColumns: TableColumn[];
}

export const createFormData = (props: CreateFormDataArgs) => {
  const { dataSourceName, table, metadata, tableColumns } = props;
  // find the specific metadata table
  const metadataTable = getMetadataTable({
    dataSourceName,
    table,
    metadata,
  });

  const roles = getRoles(metadataTable.tables);

  return {
    roles,
    supportedQueries,
    tableNames: metadataTable.tableNames,
    columns: tableColumns?.map(({ name }) => name),
  };
};

export type UseFormDataArgs = {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: 'select' | 'insert' | 'update' | 'delete';
};

type ReturnValue = {
  roles: string[];
  supportedQueries: Operation[];
  tableNames: unknown;
  columns: string[];
};

/**
 *
 * creates data for displaying in the form e.g. column names, roles etc.
 */
export const useFormData = ({ dataSourceName, table }: UseFormDataArgs) => {
  const httpClient = useHttpClient();
  return useQuery<ReturnValue, Error>({
    queryKey: [dataSourceName, 'permissionFormData'],
    queryFn: async () => {
      const metadata = await exportMetadata({ httpClient });

      // get table columns for metadata table from db introspection
      const tableColumns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      return createFormData({
        dataSourceName,
        table,
        metadata,
        tableColumns,
      });
    },
    refetchOnWindowFocus: false,
  });
};
