import { useMetadataMigration } from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { APIError } from '@/hooks/error';

interface Props {
  onSuccess?: () => void;
}

export const useCronMetadataMigration = (props: Props) => {
  const { onSuccess } = props;
  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onError: (error: APIError) => {
      fireNotification({
        type: 'error',
        title: 'Error',
        message: error?.message ?? 'Unable to create Cron Trigger',
      });
    },
    onSuccess: () => {
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: 'Cron Trigger created successfully',
      });

      onSuccess?.();
    },
  });
  return { mutation };
};
