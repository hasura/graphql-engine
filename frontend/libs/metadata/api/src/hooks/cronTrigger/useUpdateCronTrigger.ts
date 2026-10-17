import { useQueryClient } from '@tanstack/react-query';
import {
  CreateCronTriggerArgs,
  getCronTriggerCreateQuery,
} from './useCreateCronTrigger';
import { getCronTriggerDeleteQuery } from './useDeleteCronTrigger';
import { useMetadataMigrationMutation } from '../metadata';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { ALL_CRON_TRIGGERS_QUERY_KEY } from './useGetAllCronTriggers';

export const getCronTriggerUpdateQuery = (
  name: string,
  args: CreateCronTriggerArgs,
) => ({
  type: 'bulk' as const,
  args:
    !name || name === args.name
      ? // Same name: update in place, which `create_cron_trigger` only does
        // with `replace: true` (otherwise it fails with "already exists").
        [getCronTriggerCreateQuery({ ...args, replace: true })]
      : [
          getCronTriggerDeleteQuery(name),
          getCronTriggerCreateQuery({ ...args, replace: false }),
        ],
});

export const useUpdateCronTrigger = () => {
  const queryClient = useQueryClient();

  return useMetadataMigrationMutation(
    async ({ name, args }: { name: string; args: CreateCronTriggerArgs }) =>
      getCronTriggerUpdateQuery(name, args),
    {
      onError: (error) => {
        showErrorNotification({
          title: 'Update cron trigger error',
          error,
        });
      },
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success',
          message: 'Cron trigger updated successfully!',
        });

        queryClient.invalidateQueries({
          queryKey: [ALL_CRON_TRIGGERS_QUERY_KEY],
        });
      },
    },
  );
};
