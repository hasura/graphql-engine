import { useTableColumns } from '@/features/BrowseRows';
import { MetadataSelectors, useMetadata } from '@/features/hasura-metadata-api';

export const useListAllTableColumns = (
  dataSourceName: string,
  table: unknown
) => {
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
