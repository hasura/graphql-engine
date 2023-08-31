import { useTableColumns } from '../../BrowseRows';
import { TableColumn } from '../../DataSource';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { MetadataTableColumnConfig } from '../../hasura-metadata-types';
import { UseQueryResult } from 'react-query';
import { multipleQueryUtils } from '../components/ReactQueryWrappers/utils';

export type ListAllTableColumn = TableColumn & {
  config: MetadataTableColumnConfig | undefined;
};

export type ListAllTableColumnsReturn = {
  columns: ListAllTableColumn[];
} & Omit<UseQueryResult, 'data'> & {
    combinedStatus: UseQueryResult['status'];
    firstError: unknown;
  };

export const useListAllTableColumns = (
  dataSourceName: string,
  table: unknown
): ListAllTableColumnsReturn => {
  const useTableColumnsResult = useTableColumns({
    table,
    dataSourceName,
  });

  const { data: tableColumns } = useTableColumnsResult;

  const useMetadataResult = useMetadata(
    MetadataSelectors.findTable(dataSourceName, table)
  );

  const { data: metadataTable, ...rest } = useMetadataResult;

  const tableConfig = metadataTable?.configuration?.column_config;

  return {
    columns: (tableColumns?.columns ?? []).map(tableColumn => ({
      ...tableColumn,
      config: tableConfig?.[tableColumn.name],
    })),

    ...rest,
    // return a status that takes into account both queries
    combinedStatus: multipleQueryUtils.status([
      useMetadataResult,
      useTableColumnsResult,
    ]),
    // if there's any error with either request, return it here
    firstError: multipleQueryUtils.firstError([
      useMetadataResult,
      useTableColumnsResult,
    ]),
  };
};
