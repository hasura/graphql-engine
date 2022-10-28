import { buildClientSchema, GraphQLSchema } from 'graphql';
import { useQuery, UseQueryResult } from 'react-query';
import isEqual from 'lodash.isequal';

import {
  DataSource,
  exportMetadata,
  runIntrospectionQuery,
  TableColumn,
} from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { Metadata, MetadataTable } from '@/features/MetadataAPI';

import { PermissionsSchema } from '../../../utils';

import type { QueryType } from '../../../types';
import {
  createPermissionsObject,
  getRowPermissionsForAllOtherQueriesMatchingSelectedRole,
} from './utils';

interface GetMetadataTableArgs {
  dataSourceName: string;
  table: unknown;
  metadata: Metadata;
}

const getMetadataTable = ({
  dataSourceName,
  table,
  metadata,
}: GetMetadataTableArgs) => {
  const trackedTables = metadata.metadata?.sources?.find(
    source => source.name === dataSourceName
  )?.tables;

  // find selected table
  const currentTable = trackedTables?.find(trackedTable =>
    isEqual(trackedTable.table, table)
  );

  return currentTable;
};

interface CreateDefaultValuesArgs {
  queryType: QueryType;
  roleName: string;
  selectedTable?: MetadataTable;
  tableColumns: TableColumn[];
  schema: GraphQLSchema;
}

export const createDefaultValues = ({
  queryType,
  roleName,
  selectedTable,
  tableColumns,
  schema,
}: CreateDefaultValuesArgs) => {
  const allRowChecks = getRowPermissionsForAllOtherQueriesMatchingSelectedRole(
    queryType,
    roleName,
    selectedTable
  );

  const baseDefaultValues: DefaultValues = {
    checkType: 'none',
    filterType: 'none',
    check: {},
    filter: {},
    columns: {},
    presets: [],
    backendOnly: false,
    aggregationEnabled: false,
    clonePermissions: [],
    allRowChecks,
  };
  if (selectedTable) {
    const permissionsObject = createPermissionsObject({
      queryType,
      selectedTable,
      roleName,
      tableColumns,
      schema,
    });

    return { ...baseDefaultValues, ...permissionsObject };
  }

  return baseDefaultValues;
};

type DefaultValues = PermissionsSchema & {
  allRowChecks: { queryType: QueryType; value: string }[];
};

export interface Args {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: QueryType;
}

export const useDefaultValues = ({
  dataSourceName,
  table,
  roleName,
  queryType,
}: Args): UseQueryResult<DefaultValues> => {
  const httpClient = useHttpClient();
  return useQuery<any, Error>({
    queryKey: [dataSourceName, 'permissionDefaultValues', roleName, queryType],
    queryFn: async () => {
      const introspectionResult = await runIntrospectionQuery({ httpClient });
      const schema = buildClientSchema(introspectionResult.data);

      const metadata = await exportMetadata({ httpClient });

      // get table columns for metadata table from db introspection
      const tableColumns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      const selectedTable = getMetadataTable({
        dataSourceName,
        table,
        metadata,
      });

      return createDefaultValues({
        queryType,
        roleName,
        selectedTable,
        tableColumns,
        schema,
      });
    },
    refetchOnWindowFocus: false,
  });
};
