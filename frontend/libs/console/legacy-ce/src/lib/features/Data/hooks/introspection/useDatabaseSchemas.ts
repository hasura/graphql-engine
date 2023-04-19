import { useQuery } from 'react-query';
import { DataSource, Feature } from '../../../DataSource';
import { useHttpClient } from '../../../Network';

export function useGetDatabaseSchemas(dataSourceName: string) {
  const httpClient = useHttpClient();

  return useQuery<string[], Error>({
    queryKey: [dataSourceName, 'schemas'],
    queryFn: async () => {
      const result = await DataSource(httpClient).getDatabaseSchemas({
        dataSourceName,
      });

      if (result === Feature.NotImplemented) {
        throw Error(
          `getDatabaseSchemas not implemented for source: ${dataSourceName}`
        );
      } else {
        return result;
      }
    },
    refetchOnWindowFocus: false,
  });
}
