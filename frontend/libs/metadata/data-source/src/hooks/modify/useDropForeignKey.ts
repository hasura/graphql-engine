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
  getDatabaseMethods,
  ModifyForeignKeyArgs,
  NotImplementedError,
} from '../../driver';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type DropForeignKeyProps = ModifyForeignKeyArgs & {
  source: QualifiedDataSource;
};

type UseDropForeignKeyPropsOptions = Omit<
  UseMutationOptions<boolean, unknown, DropForeignKeyProps>,
  'mutationFn'
>;

export function useDropForeignKey(options?: UseDropForeignKeyPropsOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, DropForeignKeyProps>({
    ...options,
    mutationFn: async ({ source, ...props }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.dropForeignKey) {
        throw new NotImplementedError(
          `dropForeignKey not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.dropForeignKey({
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
        message: 'Foreign key dropped successfully!',
        type: 'success',
      });

      invalidateMetadata(queryClient);
      queryClient.invalidateQueries({
        queryKey: [variables.source.name],
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Dropping foreign key failed',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
