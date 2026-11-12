import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, Table } from '@hasura/shared/types';
import { getDatabaseMethods, NotImplementedError } from '../../driver';
import { invalidateMetadata } from '@hasura/metadata/api';
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

/**
 * Drops a table from the database. The server removes it from the metadata in
 * the same transaction, or fails with a `dependency-error` when other metadata
 * objects depend on it and `cascade` is false.
 *
 * Errors are not shown by this hook: callers handle them in `onError`, e.g. in
 * a dialog or a toast.
 */
export function useDropTable(options?: UseDropTableOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation<boolean, unknown, DropTableProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.dropTable) {
        throw new NotImplementedError(
          `dropTable not implemented for source: ${source.name}`,
        );
      }

      // Untracking the table in a separate request first would leave it
      // untracked whenever the DROP failed.
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

      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
