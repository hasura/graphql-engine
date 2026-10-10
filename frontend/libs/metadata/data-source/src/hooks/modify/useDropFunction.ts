import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { QualifiedDataSource, TableFunction } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { getDatabaseMethods, NotImplementedError } from '../../driver';
import { invalidateMetadata, useErrorNotification } from '@hasura/metadata/api';

type DropFunctionProps = {
  source: QualifiedDataSource;
  func: TableFunction;
};

type UseDropFunctionPropsOptions = Omit<
  UseMutationOptions<boolean, unknown, DropFunctionProps>,
  'mutationFn'
>;

export function useDropFunction(options?: UseDropFunctionPropsOptions) {
  const { endpoints, envVars } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<boolean, unknown, DropFunctionProps>({
    ...options,
    mutationFn: async ({ source, func }) => {
      const modifyMethods = getDatabaseMethods(source.kind);

      if (!modifyMethods.modify?.dropFunction) {
        throw new NotImplementedError(
          `dropFunction not implemented for source: ${source.name}`,
        );
      }

      const result = await modifyMethods.modify.dropFunction({
        dataSourceName: source.name,
        func,
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
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Function dropped successfully!',
        type: 'success',
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Dropping function failed',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
}
