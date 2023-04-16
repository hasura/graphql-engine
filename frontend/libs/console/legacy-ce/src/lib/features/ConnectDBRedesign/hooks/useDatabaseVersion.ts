import { AxiosInstance } from 'axios';
import { useQuery } from 'react-query';
import { DataSource } from '../../DataSource';
import { useHttpClient } from '../../Network';

const getDatabaseVersion = ({
  httpClient,
  dataSourceName,
}: {
  httpClient: AxiosInstance;
  dataSourceName: string;
}) => {
  return DataSource(httpClient).getDatabaseVersion(dataSourceName);
};

export const useDatabaseVersion = (
  dataSourceNames: string[],
  enabled?: boolean
) => {
  const httpClient = useHttpClient();

  return useQuery({
    queryKey: ['dbVersion', ...dataSourceNames],
    queryFn: async () => {
      const result = dataSourceNames.map(async dataSourceName => {
        try {
          const version = await getDatabaseVersion({
            dataSourceName,
            httpClient,
          });
          return {
            dataSourceName,
            version,
          };
        } catch (err) {
          return {
            dataSourceName,
          };
        }
      });
      return Promise.all(result);
    },
    enabled: enabled,
  });
};
