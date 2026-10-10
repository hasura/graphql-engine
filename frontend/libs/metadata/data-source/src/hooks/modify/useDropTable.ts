import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, NotImplementedError } from '../../driver';
import {
  getUntrackTableQuery,
  invalidateMetadata,
  runMetadataQuery,
  useErrorNotification,
} from '@hasura/metadata/api';
import { hasuraToast } from '@hasura/shared/ui';

type DropTableProps = {
  source: QualifiedDataSource;
  table: Table;
  cascade?: boolean;
};

type UseDropTableOptions = Omit<
  UseMutationOptions<boolean, unknown, DropTableProps>,
  'mutationFn'
>;

export function useDropTable(options?: UseDropTableOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, DropTableProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.dropTable) {
        throw new NotImplementedError(
          `dropTable not implemented for source: ${source.name}`,
        );
      }

      await runMetadataQuery({
        fetchJson,
        url: endpoints.metadata,
        body: getUntrackTableQuery(
          {
            source: source.name,
            ...props,
          },
          source.kind,
        ),
      });

      const result = await modifyMethods.modify.dropTable({
        dataSourceName: source.name,
        isMigration: envVars.consoleMode === 'cli',
        endpoints,
        fetchJson,
        ...props,
      });

      return result;
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Table dropped successfully!',
        type: 'success',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({
        queryKey: [variables.source.name],
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError(error, variables, onMutateResult, context) {
      invalidateMetadata(queryClient);
      showErrorNotification({
        title: 'Dropping table failed',
        error,
      });

      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
