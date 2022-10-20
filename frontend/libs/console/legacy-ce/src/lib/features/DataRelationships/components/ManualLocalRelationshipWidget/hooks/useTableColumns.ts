import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { DataSource, TableColumn } from '@/features/DataSource';
import { Table } from '@/features/MetadataAPI';
import { AxiosError } from 'axios';

export const useTableColumns = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const httpClient = useHttpClient();
  return useQuery<TableColumn[], AxiosError>({
    queryKey: ['tableColumns', dataSourceName, table],
    queryFn: () => {
      const columns = DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });
      return columns;
    },
    refetchOnWindowFocus: false,
    enabled: !!table && !!dataSourceName,
  });
};
