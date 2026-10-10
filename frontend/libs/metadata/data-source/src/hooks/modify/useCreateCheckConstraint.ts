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
  CreateCheckConstraintArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type CreateCheckConstraintProps = CreateCheckConstraintArgs & {
  source: QualifiedDataSource;
};

type UseCreateCheckConstraintOptions = Omit<
  UseMutationOptions<boolean, unknown, CreateCheckConstraintProps>,
  'mutationFn'
>;

export function useCreateCheckConstraint(
  options?: UseCreateCheckConstraintOptions,
) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, CreateCheckConstraintProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.createCheckConstraint) {
        throw new NotImplementedError(
          `createCheckConstraint not implemented for source: ${source.name}`,
        );
      }

      return modifyMethods.modify.createCheckConstraint({
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
        message: 'Check constraint created successfully!',
        type: 'success',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({ queryKey: [variables.source.name] });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Creating check constraint failed',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
