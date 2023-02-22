import { useCallback } from 'react';
import { Table } from '../../hasura-metadata-types';
import {
  generateGraphQLDeleteByPrimaryKeyMutation,
  generateGraphQLDeleteMutation,
} from '../../GraphQLUtils';
import { useTableColumns } from '../../BrowseRows';
import { useGraphQLMutation, useDefaultQueryRoot } from '../../Data';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { TableRow } from '../../DataSource';
import { useSelector } from 'react-redux';

export function useDeleteRow({
  dataSourceName,
  table,
  onError,
  onSuccess,
}: {
  dataSourceName: string;
  table: Table;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) {
  const { data: source } = useMetadata(
    MetadataSelectors.findSource(dataSourceName)
  );

  const { data } = useTableColumns({ table, dataSourceName });
  const primaryKeys = data?.columns
    .filter(column => column.isPrimaryKey)
    .map(column => column.name);

  // Needed to avoid circular dependency
  const headers = useSelector<any>(state => state.tables.dataHeaders) as Record<
    string,
    string
  >;
  const { data: tableData } = useMetadata(
    MetadataSelectors.findTable(dataSourceName, table)
  );
  const { mutate, ...rest } = useGraphQLMutation({
    operationName: 'DeleteRow',
    headers,
    onError,
    onSuccess,
  });
  const defaultQueryRoot = useDefaultQueryRoot({ dataSourceName, table });

  const deleteRow = useCallback(
    async (row: TableRow) => {
      mutate(
        primaryKeys
          ? generateGraphQLDeleteByPrimaryKeyMutation({
              row,
              defaultQueryRoot,
              mutationName: 'DeleteRow',
              tableCustomization: tableData?.configuration,
              sourceCustomization: source?.customization,
              primaryKeys,
            })
          : generateGraphQLDeleteMutation({
              rows: [row],
              defaultQueryRoot,
              mutationName: 'DeleteRow',
              tableCustomization: tableData?.configuration,
              sourceCustomization: source?.customization,
            })
      );
    },
    [
      mutate,
      source?.customization,
      tableData?.configuration,
      primaryKeys,
      defaultQueryRoot,
    ]
  );
  return {
    deleteRow,
    ...rest,
  };
}
