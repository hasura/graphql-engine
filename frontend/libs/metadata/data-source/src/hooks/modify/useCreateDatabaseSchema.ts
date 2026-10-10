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

type CreateDatabaseSchemaProps = {
  source: QualifiedDataSource;
  schemaName: string;
};

type UseCreateDatabaseSchemaOptions = Omit<
  UseMutationOptions<RunSQLResponse | null, unknown, CreateDatabaseSchemaProps>,
  'mutationFn'
>;

export function useCreateDatabaseSchema(
  options?: UseCreateDatabaseSchemaOptions,
) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation({
    ...options,
    mutationFn: async ({ source, schemaName }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.createDatabaseSchema) {
        throw new NotImplementedError(
          `createDatabaseSchema not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.createDatabaseSchema({
        dataSourceName: source.name,
        schemaName,
        endpoints,
        fetchJson,
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
