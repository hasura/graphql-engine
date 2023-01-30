import { GraphQLSchema } from 'graphql';

import isEqual from 'lodash.isequal';

import { TableColumn } from '@/features/DataSource';
import { getTypeName } from '@/features/GraphQLUtils';

import { Metadata } from '@/features/hasura-metadata-types';

import { PermissionsSchema } from '../../../../../schema';

import type { QueryType } from '../../../../../types';
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

interface Args {
  queryType: QueryType;
  roleName: string;
  table: unknown;
  dataSourceName: string;
  metadata: Metadata;
  tableColumns: TableColumn[];
  schema: GraphQLSchema;
  defaultQueryRoot: string | never[];
}

export const createDefaultValues = ({
  queryType,
  roleName,
  table,
  dataSourceName,
  metadata,
  tableColumns,
  schema,
  defaultQueryRoot,
}: Args) => {
  const selectedTable = getMetadataTable({
    dataSourceName,
    table,
    metadata,
  });

  const metadataSource = metadata.metadata.sources.find(
    s => s.name === dataSourceName
  );

  /**
   * This is GDC specific, we have to move this to DAL later
   */

  const tableName = getTypeName({
    defaultQueryRoot,
    operation: 'select',
    sourceCustomization: metadataSource?.customization,
    configuration: selectedTable?.configuration,
  });

  const allRowChecks = getRowPermissionsForAllOtherQueriesMatchingSelectedRole(
    queryType,
    roleName,
    selectedTable
  );

  const baseDefaultValues: DefaultValues = {
    queryType: 'select',
    filterType: 'none',
    columns: {},
    allRowChecks,
  };
  if (selectedTable) {
    const permissionsObject = createPermissionsObject({
      queryType,
      selectedTable,
      roleName,
      tableColumns,
      schema,
      tableName,
    });

    return { ...baseDefaultValues, ...permissionsObject };
  }

  return baseDefaultValues;
};

type DefaultValues = PermissionsSchema & {
  allRowChecks: { queryType: QueryType; value: string }[];
  operators?: Record<string, unknown>;
};
