import React from 'react';

import {
  useTrackableFunctions,
  useDataSourceTables,
  useSingleTable,
} from '@/hooks';
import { useAppSelector } from '@/store';
import { currentDriver, dataSource } from '@/dataSources';

import { useRoles } from '../../../MetadataAPI';
import { QueryType } from '../../types';

export interface UseFormDataArgs {
  schemaName: string;
  tableName: string;
  roleName: string;
  queryType: QueryType;
}

const useLoadSchemas = ({ schemaName, tableName }: UseFormDataArgs) => {
  const source: string = useAppSelector(
    state => state.tables.currentDataSource
  );

  const {
    data: tables,
    isLoading: tablesLoading,
    isError: tablesError,
  } = useDataSourceTables(
    { schemas: [schemaName], source, driver: currentDriver },
    {
      enabled: !!schemaName,
      retry: 0,
    }
  );

  const {
    data: table,
    isLoading: tableLoading,
    isError: tableError,
  } = useSingleTable({
    source,
    driver: currentDriver,
    table: { name: tableName, schema: schemaName },
  });
  const { data: roles } = useRoles();

  const { data: allFunctions } = useTrackableFunctions(
    { schemas: [schemaName], source, driver: currentDriver },
    {
      enabled: !!schemaName,
      retry: 0,
    }
  );

  const isError = tablesError || tableError;
  const isLoading = tablesLoading || tableLoading;

  if (isError) {
    return {
      data: {},
      isLoading: false,
      isError: true,
    };
  }

  if (isLoading) {
    return {
      data: {},
      isLoading: true,
      isError: false,
    };
  }

  return {
    data: { tables, table, roles, allFunctions },
    isLoading: false,
    isError: false,
  };
};

export const useFormData = (props: UseFormDataArgs) => {
  const {
    data: { table, tables, roles, allFunctions },
    isLoading,
    isError,
  } = useLoadSchemas(props);

  const otherTableNames = React.useMemo(
    () =>
      tables
        ?.filter(({ table_name }) => table_name !== table?.table_name)
        .map(({ table_name }) => table_name),
    [tables, table]
  );
  const columns = React.useMemo(
    () => table?.columns.map(({ column_name }) => column_name),
    [table]
  );

  let supportedQueries: string[] = [];
  if (table) {
    // supportedQueries = ['insert', 'select', 'update', 'delete'];
    supportedQueries = dataSource.getTableSupportedQueries(table);
  }

  return {
    data: {
      table,
      tables,
      otherTableNames,
      columns,
      allFunctions,
      roles,
      supportedQueries,
    },
    isLoading,
    isError,
  };
};
