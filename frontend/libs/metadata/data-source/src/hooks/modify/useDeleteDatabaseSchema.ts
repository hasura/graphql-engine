import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, RunSQLResponse } from '@hasura/shared/types';
import { getDatabaseMethods, NotImplementedError } from '../../driver';
import { invalidateMetadata } from '@hasura/metadata/api';

type DeleteDatabaseSchemaProps = {
  source: QualifiedDataSource;
  schemaName: string;
};

type UseDeleteDatabaseSchemaOptions = Omit<
  UseMutationOptions<RunSQLResponse | null, unknown, DeleteDatabaseSchemaProps>,
  'mutationFn'
>;

export function useDeleteDatabaseSchema(
  options?: UseDeleteDatabaseSchemaOptions,
) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation({
    ...options,
    mutationFn: async ({ schemaName, source }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.deleteDatabaseSchema) {
        throw new NotImplementedError(
          `deleteDatabaseSchema not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.deleteDatabaseSchema({
        dataSourceName: source.name,
        fetchJson,
        schemaName,
        endpoints,
        isMigration: envVars.consoleMode === 'cli',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({
        queryKey: [source.name],
      });

      return result;
    },
  });
}
