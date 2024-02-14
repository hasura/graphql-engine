import { AxiosError } from 'axios';
import { useMutation, useQueryClient } from 'react-query';
import {
  DataSource,
  Feature,
  RunSQLAPIError,
  RunSQLResponse,
} from '../../../DataSource';
import { useHttpClient } from '../../../Network';

export function useCreateDatabaseSchema({
  dataSourceName,
}: {
  dataSourceName: string;
}) {
  const httpClient = useHttpClient();
  const queryClient = useQueryClient();

  return useMutation<
    RunSQLResponse | Feature.NotImplemented,
    AxiosError<RunSQLAPIError>,
    { schemaName: string }
  >({
    mutationFn: async ({ schemaName }) => {
      const modifyMethods = await DataSource(httpClient).modifyDatabase({
        dataSourceName,
      });

      const result = await modifyMethods.createDatabaseSchema({
        dataSourceName,
        httpClient,
        schemaName,
      });

      if (result === Feature.NotImplemented) {
        throw Error(
          `createDatabaseSchema not implemented for source: ${dataSourceName}`
        );
      } else {
        queryClient.invalidateQueries([dataSourceName, 'schemas']);
        return result;
      }
    },
  });
}
