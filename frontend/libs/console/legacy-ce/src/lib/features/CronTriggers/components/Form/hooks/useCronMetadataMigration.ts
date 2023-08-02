import { useMetadataMigration } from '../../../../MetadataAPI';
import { useFireNotification } from '../../../../../new-components/Notifications';
import { APIError } from '../../../../../hooks/error';
import { CRON_TRIGGERS_QUERY_KEY } from '../../../../../components/Services/Events/CronTriggers/Hooks/useGetCronTriggers';
import { useQueryClient } from 'react-query';
import { ALL_CRON_TRIGGERS_QUERY_KEY } from './useGetAllCronTriggers';

interface Props {
  onSuccess?: () => void;
  triggerName?: string;
  successMessage: string;
  errorMessage: string;
}

export const useCronMetadataMigration = (props: Props) => {
  const { onSuccess, successMessage, errorMessage } = props;
  const { fireNotification } = useFireNotification();
  const queryClient = useQueryClient();
  const mutation = useMetadataMigration({
    onError: (error: APIError) => {
      fireNotification({
        type: 'error',
        title: 'Error',
        message: `${errorMessage}: ${error.message}`,
      });
    },
    onSuccess: () => {
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: successMessage,
      });

      /* this query is performed in legacy section, remove it will completely be refactored */
      queryClient.invalidateQueries({
        queryKey: [CRON_TRIGGERS_QUERY_KEY],
      });
      queryClient.invalidateQueries({
        queryKey: [ALL_CRON_TRIGGERS_QUERY_KEY],
      });
      onSuccess?.();
    },
  });
  return { mutation };
};
