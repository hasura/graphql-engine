import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { hasuraToast } from '@hasura/shared/ui';
import {
  DeleteRowArgs,
  getDatabaseMethods,
  NotImplementedError,
} from '../../driver';
import { getBrowseRowsQueryKey } from './useRows';
import { useErrorNotification } from '@hasura/metadata/api';

type MutationOptions = Omit<
  UseMutationOptions<number, unknown, DeleteRowArgs>,
  'mutationFn'
>;

export const useDeleteRows = (options?: MutationOptions) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<number, unknown, DeleteRowArgs>({
    ...options,
    mutationFn: (args) => {
      const sourceAPI = getDatabaseMethods(args.source.kind);
      if (!sourceAPI.modify?.deleteRows) {
        throw new NotImplementedError(
          `Source '${args.source.name}' does not support deleting rows`,
        );
      }

      return sourceAPI.modify.deleteRows({
        endpoints,
        fetchJson,
        args,
      });
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Row deleted successfully!',
        type: 'success',
      });

      queryClient.invalidateQueries({
        queryKey: getBrowseRowsQueryKey(variables),
      });

      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Error while deleting the row',
        error,
      });

      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
};
