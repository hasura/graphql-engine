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
  NotImplementedError,
  UpdateRowArgs,
} from '../../driver';
import { getBrowseRowsQueryKey } from './useRows';
import { useErrorNotification } from '@hasura/metadata/api';

type EditRowProps = UpdateRowArgs;

type MutationOptions = Omit<
  UseMutationOptions<number, unknown, EditRowProps>,
  'mutationFn'
>;

export const useEditRows = (options?: MutationOptions) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const queryClient = useQueryClient();
  const showErrorNotification = useErrorNotification();

  return useMutation<number, unknown, EditRowProps>({
    ...options,
    mutationFn: (args) => {
      const sourceAPI = getDatabaseMethods(args.source.kind);
      if (!sourceAPI.modify?.updateRows) {
        throw new NotImplementedError(
          `Source '${args.source.name}' does not support editing rows`,
        );
      }

      return sourceAPI.modify.updateRows({
        endpoints,
        fetchJson,
        args,
      });
    },
    onSuccess: (data, variables, onMutateResult, context) => {
      hasuraToast({
        title: 'Success!',
        message: 'Row updated successfully!',
        type: 'success',
      });

      queryClient.invalidateQueries({
        queryKey: getBrowseRowsQueryKey(variables),
      });
      options?.onSuccess?.(data, variables, onMutateResult, context);
    },
    onError: (error, variables, onMutateResult, context) => {
      showErrorNotification({
        title: 'Error while updating the row',
        error,
      });
      options?.onError?.(error, variables, onMutateResult, context);
    },
  });
};
