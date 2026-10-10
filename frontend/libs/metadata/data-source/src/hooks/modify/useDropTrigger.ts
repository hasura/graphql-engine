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
  DropTriggerArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type Props = DropTriggerArgs & { source: QualifiedDataSource };
type Options = Omit<UseMutationOptions<boolean, unknown, Props>, 'mutationFn'>;

export function useDropTrigger(options?: Options) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, Props>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);
      if (!modifyMethods.modify?.dropTrigger) {
        throw new NotImplementedError(
          `dropTrigger not implemented for source: ${source.name}`,
        );
      }
      return modifyMethods.modify.dropTrigger({
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
        message: 'Trigger removed successfully!',
        type: 'success',
      });
      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({ queryKey: [variables.source.name] });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({ title: 'Removing trigger failed', error });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
