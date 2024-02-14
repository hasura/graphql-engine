import { AxiosError } from 'axios';
import { useMutation, useQueryClient } from 'react-query';
import {
  DataSource,
  Feature,
  RunSQLAPIError,
  RunSQLResponse,
} from '../../../DataSource';
import { useHttpClient } from '../../../Network';

export function useDeleteDatabaseSchema({
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

      const result = await modifyMethods.deleteDatabaseSchema({
        dataSourceName,
        httpClient,
        schemaName,
      });

      if (result === Feature.NotImplemented) {
        throw Error(
          `deleteDatabaseSchema not implemented for source: ${dataSourceName}`
        );
      } else {
        queryClient.invalidateQueries([dataSourceName, 'schemas']);
        queryClient.invalidateQueries([dataSourceName, 'untracked_tables']);
        return result;
      }
    },
  });
}
