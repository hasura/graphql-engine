import { useQueryClient } from '@tanstack/react-query';
import { ALL_CRON_TRIGGERS_QUERY_KEY } from './useGetAllCronTriggers';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from '../metadata';

export const getCronTriggerDeleteQuery = (name: string) => ({
  type: 'delete_cron_trigger' as const,
  args: {
    name,
  },
});

export const useDeleteCronTrigger = () => {
  const queryClient = useQueryClient();

  return useMetadataMigrationMutation(
    async (name: string) => getCronTriggerDeleteQuery(name),
    {
      onError: (error) => {
        showErrorNotification({
          title: 'Deleting cron trigger error',
          error,
        });
      },
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success',
          message: 'Cron trigger deleted successfully',
        });

        queryClient.invalidateQueries({
          queryKey: [ALL_CRON_TRIGGERS_QUERY_KEY],
        });
      },
    },
  );
};
