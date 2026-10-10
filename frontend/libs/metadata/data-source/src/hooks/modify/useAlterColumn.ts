import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import {
  AlterColumnArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata } from '@hasura/metadata/api';

type Props = AlterColumnArgs & { source: QualifiedDataSource };

type Options = Omit<UseMutationOptions<boolean, unknown, Props>, 'mutationFn'>;

/** Resolves `false` (without running anything) when no column change is
 *  detected, `true` once the migration has been applied. */
export function useAlterColumn(options?: Options) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation<boolean, unknown, Props>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);
      if (!modifyMethods.modify?.alterColumn) {
        throw new NotImplementedError(
          `alterColumn not implemented for source: ${source.name}`,
        );
      }
      return modifyMethods.modify.alterColumn({
        dataSourceName: source.name,
        isMigration: envVars.consoleMode === 'cli',
        endpoints,
        fetchJson,
        ...props,
      });
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      if (data) {
        hasuraToast({
          title: 'Success!',
          message: 'Column modified successfully!',
          type: 'success',
        });
        invalidateMetadata(queryClient);
        queryClient.invalidateQueries({ queryKey: [variables.source.name] });
      }
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
  });
}
