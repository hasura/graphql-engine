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
  CreateTriggerArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata } from '@hasura/metadata/api';

type Props = CreateTriggerArgs & { source: QualifiedDataSource };

type Options = Omit<UseMutationOptions<boolean, unknown, Props>, 'mutationFn'>;

export function useCreateTrigger(options?: Options) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();

  return useMutation<boolean, unknown, Props>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);
      if (!modifyMethods.modify?.createTrigger) {
        throw new NotImplementedError(
          `createTrigger not implemented for source: ${source.name}`,
        );
      }
      return modifyMethods.modify.createTrigger({
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
        message: 'Trigger created successfully!',
        type: 'success',
      });
      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({ queryKey: [variables.source.name] });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
  });
}
