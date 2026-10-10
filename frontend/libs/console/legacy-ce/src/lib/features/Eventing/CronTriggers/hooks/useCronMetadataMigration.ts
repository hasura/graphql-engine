import { useMetadataMigration } from '@hasura/metadata/api';
import { useQueryClient } from '@tanstack/react-query';
import { ALL_CRON_TRIGGERS_QUERY_KEY } from './useGetAllCronTriggers';
import { hasuraToast } from '@hasura/shared/ui';

interface Props {
  onSuccess?: () => void;
  triggerName?: string;
  successMessage: string;
  errorMessage: string;
}

export const useCronMetadataMigration = (props: Props) => {
  const { onSuccess, successMessage, errorMessage } = props;
  const queryClient = useQueryClient();
  const mutation = useMetadataMigration({
    onError: (error) => {
      hasuraToast({
        type: 'error',
        title: 'Error',
        message: `${errorMessage}: ${error.message}`,
      });
    },
    onSuccess: () => {
      hasuraToast({
        type: 'success',
        title: 'Success!',
        message: successMessage,
      });

      queryClient.invalidateQueries({
        queryKey: [ALL_CRON_TRIGGERS_QUERY_KEY],
      });
      onSuccess?.();
    },
  });
  return { mutation };
};
