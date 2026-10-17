import { useQueryClient } from '@tanstack/react-query';
import { ALL_CRON_TRIGGERS_QUERY_KEY } from './useGetAllCronTriggers';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from '../metadata';
import { CronTrigger } from '@hasura/shared/types';

export type CreateCronTriggerArgs = CronTrigger & {
  // When replace is set to true, the cron trigger will be updated(if exists)
  // and when it's false or the field is omitted, then a new cron trigger will be created.
  replace?: boolean;
};

export const getCronTriggerCreateQuery = (args: CreateCronTriggerArgs) => {
  return {
    type: 'create_cron_trigger' as const,
    args,
  };
};

export const useCreateCronTrigger = () => {
  const queryClient = useQueryClient();

  return useMetadataMigrationMutation(
    async (args: CreateCronTriggerArgs) => getCronTriggerCreateQuery(args),
    {
      onError: (error) => {
        showErrorNotification({
          title: 'Creating cron trigger error',
          error,
        });
      },
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Success',
          message: 'Cron trigger created successfully!',
        });

        queryClient.invalidateQueries({
          queryKey: [ALL_CRON_TRIGGERS_QUERY_KEY],
        });
      },
    },
  );
};
