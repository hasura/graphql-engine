import { useMetadataMigration } from '../../../../MetadataAPI';
import { useFireNotification } from '../../../../../new-components/Notifications';
import { APIError } from '../../../../../hooks/error';

interface Props {
  onSuccess?: () => void;
}

export const useOneOffScheduledTriggerMigration = (props: Props) => {
  const { onSuccess } = props;
  const { fireNotification } = useFireNotification();
  const mutation = useMetadataMigration({
    onError: (error: APIError) => {
      fireNotification({
        type: 'error',
        title: 'Error',
        message: error?.message ?? 'Unable to schedule Event',
      });
    },
    onSuccess: () => {
      fireNotification({
        type: 'success',
        title: 'Success!',
        message: 'Event scheduled successfully',
      });

      onSuccess?.();
    },
  });
  return { mutation };
};
