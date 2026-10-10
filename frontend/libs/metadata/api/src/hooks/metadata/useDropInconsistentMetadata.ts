import { useCallback } from 'react';
import { hasuraToast } from '@hasura/shared/ui';
import { useMetadataMigration } from './useMetadataMigration';
import { useErrorNotification } from '../notification';

export const useDropInconsistentMetadata = () => {
  const mutation = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  return useCallback(async () => {
    mutation.mutate(
      {
        query: {
          type: 'drop_inconsistent_metadata',
          args: {},
        },
      },
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
  }, [mutation]);
};
