import { useTableColumns } from '../../BrowseRows';
import { TableColumn } from '../../DataSource';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { MetadataTableColumnConfig } from '../../hasura-metadata-types';
import { UseQueryResult } from 'react-query';

export type ListAllTableColumn = TableColumn & {
  config: MetadataTableColumnConfig | undefined;
};

export type ListAllTableColumnsReturn = {
  columns: ListAllTableColumn[];
} & Omit<UseQueryResult, 'data'>;

export const useListAllTableColumns = (
  dataSourceName: string,
  table: unknown
): ListAllTableColumnsReturn => {
  const { data: tableColumns } = useTableColumns({
    table,
    dataSourceName,
  });

  const { data: metadataTable, ...rest } = useMetadata(
    MetadataSelectors.findTable(dataSourceName, table)
  );

  const tableConfig = metadataTable?.configuration?.column_config;

  return {
    columns: (tableColumns?.columns ?? []).map(tableColumn => ({
      ...tableColumn,
      config: tableConfig?.[tableColumn.name],
    })),
    ...rest,
  };
};
