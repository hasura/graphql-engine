import { DataSource } from '../../DataSource';
import { useHttpClient } from '../../Network';
import { useQuery } from 'react-query';

export const useIsTableView = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: unknown;
}) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: [dataSourceName, table, 'isView'],
    queryFn: async () => {
      const isView = await DataSource(httpClient).getIsTableView({
        dataSourceName,
        table,
        httpClient,
      });

      return isView;
    },
    refetchOnWindowFocus: false,
  });
};
