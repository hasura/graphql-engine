import { useMetadataMigration } from '@hasura/metadata/api';
import { hasuraToast } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';

interface Props {
  onSuccess?: () => void;
}

export const useOneOffScheduledTriggerMigration = (props: Props) => {
  const { onSuccess } = props;
  const mutation = useMetadataMigration({
    onError: (error: unknown) => {
      hasuraToast({
        type: 'error',
        title: 'Error',
        message: getErrorMessage(error) ?? 'Unable to schedule Event',
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
