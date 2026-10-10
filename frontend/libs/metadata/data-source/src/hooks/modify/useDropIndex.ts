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
  DropIndexArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type Props = DropIndexArgs & { source: QualifiedDataSource };
type Options = Omit<UseMutationOptions<boolean, unknown, Props>, 'mutationFn'>;

export function useDropIndex(options?: Options) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, Props>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);
      if (!modifyMethods.modify?.dropIndex) {
        throw new NotImplementedError(
          `dropIndex not implemented for source: ${source.name}`,
        );
      }
      return modifyMethods.modify.dropIndex({
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
        message: 'Index removed successfully!',
        type: 'success',
      });
      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({ queryKey: [variables.source.name] });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({ title: 'Removing index failed', error });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
