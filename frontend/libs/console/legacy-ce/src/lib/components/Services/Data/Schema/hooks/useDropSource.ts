import { Source } from '../../../../../features/hasura-metadata-types';
import { useMetadataMigration } from '../../../../../features/MetadataAPI';
import { useFireNotification } from '../../../../../new-components/Notifications';
import { useCallback } from 'react';

interface UseDropSourceProps {
  customOnSuccess?: () => void;
  customOnError?: (err: Error) => void;
}
export const useDropSource = (props?: UseDropSourceProps) => {
  const mutation = useMetadataMigration();
  const { customOnSuccess, customOnError } = props ?? {};

  const { fireNotification } = useFireNotification();
  const dropSource = useCallback(
    async (driver: Source['kind'], name: Source['name']) => {
      mutation.mutate(
        {
          query: {
            type: `${driver}_drop_source`,
            args: {
              name,
              cascade: true,
            },
          },
        },
        {
          onSuccess: () => {
            fireNotification({
              title: 'Success!',
              message: 'Successfully removed source from Hasura',
              type: 'success',
            });

            if (customOnSuccess) {
              customOnSuccess();
            }
          },
          onError: err => {
            fireNotification({
              title: 'Error!',
              message: JSON.stringify(err),
              type: 'error',
            });
            if (customOnError) {
              customOnError(err);
            }
          },
        }
      );
    },
    [customOnError, customOnSuccess, fireNotification, mutation]
  );
  return { dropSource, isLoading: mutation.isLoading };
};
