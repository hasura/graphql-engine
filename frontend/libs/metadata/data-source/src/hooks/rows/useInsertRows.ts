import {
  useMutation,
  UseMutationOptions,
  useQueryClient,
} from '@tanstack/react-query';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useAppContext } from '@hasura/shared/context';
import { hasuraToast } from '@hasura/shared/ui';
import {
  getDatabaseMethods,
  InsertRowArgs,
  NotImplementedError,
} from '../../driver';
import { getBrowseRowsQueryKey } from './useRows';
import { useErrorNotification } from '@hasura/metadata/api';

type InsertRowProps = InsertRowArgs;

type MutationOptions = Omit<
  UseMutationOptions<number, unknown, InsertRowProps>,
  'mutationFn'
>;

export const useInsertRows = (options?: MutationOptions) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<number, unknown, InsertRowProps>({
    ...options,
    mutationFn: (args) => {
      const sourceAPI = getDatabaseMethods(args.source.kind);
      if (!sourceAPI.modify?.insertRows) {
        throw new NotImplementedError(
          `Source '${args.source.name}' does not support inserting rows`,
        );
      }

      return sourceAPI.modify.insertRows({
        endpoints,
        fetchJson,
        args,
      });
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Row inserted successfully!',
        type: 'success',
      });

      queryClient.invalidateQueries({
        queryKey: getBrowseRowsQueryKey(variables),
      });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Error while inserting the row',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
};
