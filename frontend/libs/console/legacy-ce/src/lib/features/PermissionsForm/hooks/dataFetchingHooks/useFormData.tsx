import React from 'react';

import {
  useTrackableFunctions,
  useDataSourceTables,
  useSingleTable,
} from '@/hooks';
import { currentDriver, dataSource } from '@/dataSources';

import { useRoles } from '../../../MetadataAPI';
import { QueryType } from '../../types';
import { NewDataTarget } from '../../../PermissionsTab/types/types';

export interface UseFormDataArgs {
  dataTarget: NewDataTarget;
  roleName: string;
  queryType: QueryType;
}

const useLoadSchemas = ({ dataTarget }: UseFormDataArgs) => {
  const schemaName = dataTarget.dataLeaf.name;
  const tableName = dataTarget.dataLeaf.leaf?.name || '';

  const source = dataTarget.dataSource.database;

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
    data: { table, tables: allTables, roles, allFunctions },
    isLoading,
    isError,
  } = useLoadSchemas(props);

  const tables = React.useMemo(
    () => allTables?.filter(({ is_table_tracked }) => is_table_tracked),
    [allTables]
  );

  const tableNames = React.useMemo(
    () => tables?.map(({ table_name }) => table_name),
    [tables]
  );

  const columns = React.useMemo(
    () => table?.columns.map(({ column_name }) => column_name),
    [table]
  );

  let supportedQueries: string[] = [];
  if (table) {
    supportedQueries = dataSource.getTableSupportedQueries(table);
  }

  return {
    data: {
      table,
      tables,
      tableNames,
      columns,
      allFunctions,
      roles,
      supportedQueries,
    },
    isLoading,
    isError,
  };
};
