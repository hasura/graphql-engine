import { useMetadataMigration } from '@hasura/metadata/api';
import { hasuraToast, showErrorNotification } from '@hasura/shared/ui';

interface Props {
  onSuccess?: () => void;
}

export const useOneOffScheduledTriggerMigration = (props: Props) => {
  const { onSuccess } = props;
  const mutation = useMetadataMigration({
    onError: (error: unknown) => {
      showErrorNotification({
        title: 'Unable to schedule Event',
        error,
      });
    },
    onSuccess: () => {
      hasuraToast({
        type: 'success',
        title: 'Success!',
        message: 'Event scheduled successfully',
      });

      onSuccess?.();
    },
  });
  return { mutation };
};
