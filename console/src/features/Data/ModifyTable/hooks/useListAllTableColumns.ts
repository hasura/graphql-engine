// eslint-disable-next-line no-restricted-imports
import { useTableColumns } from '@/features/BrowseRows/components/DataGrid/useTableColumns';
import { exportMetadata } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { areTablesEqual } from '@/features/RelationshipsTable';
import { useQuery } from 'react-query';

export const useListAllTableColumns = ({
  table,
  dataSourceName,
}: {
  table: Table;
  dataSourceName: string;
}) => {
  const httpClient = useHttpClient();

  const { data: tableColumns, isSuccess: isIntrospectionReady } =
    useTableColumns({
      table,
      dataSourceName,
    });

  return useQuery({
    queryKey: ['modify', 'columns', dataSourceName, table],
    queryFn: async () => {
      const { metadata } = await exportMetadata({ httpClient });

      const metadataTable = metadata?.sources
        .find(s => s.name === dataSourceName)
        ?.tables.find(t => areTablesEqual(t.table, table));

      const tableConfig = metadataTable?.configuration?.column_config;
      return (tableColumns?.columns ?? []).map(tableColumn => {
        return {
          ...tableColumn,
          config: tableConfig?.[tableColumn.name],
        };
      });
    },
    enabled: isIntrospectionReady,
  });
};
