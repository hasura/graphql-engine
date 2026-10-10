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
  DropCheckConstraintArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type DropCheckConstraintProps = DropCheckConstraintArgs & {
  source: QualifiedDataSource;
};

type UseDropCheckConstraintOptions = Omit<
  UseMutationOptions<boolean, unknown, DropCheckConstraintProps>,
  'mutationFn'
>;

export function useDropCheckConstraint(
  options?: UseDropCheckConstraintOptions,
) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, DropCheckConstraintProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.dropCheckConstraint) {
        throw new NotImplementedError(
          `dropCheckConstraint not implemented for source: ${source.name}`,
        );
      }

      return modifyMethods.modify.dropCheckConstraint({
        dataSourceName: source.name,
        isMigration: envVars.consoleMode === 'cli',
        endpoints,
        fetchJson,
        ...props,
      });
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Check constraint removed successfully!',
        type: 'success',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({ queryKey: [variables.source.name] });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Removing check constraint failed',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
