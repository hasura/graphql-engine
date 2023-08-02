import { useHttpClient } from '../../Network';
import { useQuery } from 'react-query';
import { DataSource, TableColumn } from '../../DataSource';
import { Table } from '../../hasura-metadata-types';
import { AxiosError } from 'axios';
import { generateQueryKeys } from '../utils/queryClientUtils';

export const useTableColumns = ({
  dataSourceName,
  table,
  enabled = true,
}: {
  dataSourceName: string;
  table: Table;
  enabled?: boolean;
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
