import { Source } from '../../../../../features/hasura-metadata-types';
import { useMetadataMigration } from '../../../../../features/MetadataAPI';
import { useFireNotification } from '../../../../../new-components/Notifications';
import { useCallback } from 'react';

type UseReloadSource = {
  customOnSuccess?: () => void;
  customOnError?: (err: Error) => void;
};
export const useReloadSource = (props?: UseReloadSource) => {
  const mutation = useMetadataMigration();
  const { customOnSuccess, customOnError } = props ?? {};

  const { fireNotification } = useFireNotification();
  const reloadSource = useCallback(
    async (name: Source['name']) => {
      mutation.mutate(
        {
          query: {
            type: 'reload_metadata',
            args: { reload_sources: [name] },
          },
        },
        {
          onSuccess: () => {
            fireNotification({
              title: 'Success!',
              message: 'Successfully reloaded source!',
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
  return { reloadSource, isLoading: mutation.isLoading };
};
