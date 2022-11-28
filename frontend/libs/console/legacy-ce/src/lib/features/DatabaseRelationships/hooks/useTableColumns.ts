import { useHttpClient } from '@/features/Network';
import { useQuery } from 'react-query';
import { DataSource, TableColumn } from '@/features/DataSource';
import { Table } from '@/features/hasura-metadata-types';
import { AxiosError } from 'axios';
import { generateQueryKeys } from '../utils/queryClientUtils';

export const useTableColumns = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const httpClient = useHttpClient();
  return useQuery<TableColumn[], AxiosError>({
    queryKey: generateQueryKeys.columns({ dataSourceName, table }),
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
