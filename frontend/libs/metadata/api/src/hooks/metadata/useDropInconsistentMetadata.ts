import { hasuraToast } from '@hasura/shared/ui';
import { useMetadataMigrationMutation } from './useMetadataMigration';
import { useErrorNotification } from '../notification';

export const useDropInconsistentMetadata = () => {
  const showErrorNotification = useErrorNotification();

  return useMetadataMigrationMutation(
    async () => ({
      type: 'drop_inconsistent_metadata',
      args: {},
    }),
    {
      onSuccess: () => {
        hasuraToast({
          type: 'success',
          title: 'Dropped inconsistent metadata',
        });
      },
      onError: (err) => {
        showErrorNotification({
          title: 'Dropping inconsistent metadata failed',
          error: err,
        });
      },
    },
  );
};
