import { DataSource } from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';
import { AxiosError } from 'axios';
import { useQuery } from 'react-query';

export const useDatabaseHierarchy = (dataSourceName: string) => {
  const httpClient = useHttpClient();
  return useQuery<string[], AxiosError>({
    queryKey: [dataSourceName, 'hierarchy'],
    queryFn: async () => {
      const hierarcy = await DataSource(httpClient).getDatabaseHierarchy({
        dataSourceName,
      });
      return hierarcy;
    },
    refetchOnWindowFocus: false,
  });
};
