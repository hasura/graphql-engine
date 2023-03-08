import { useInvalidateMetadata } from '../../hasura-metadata-api';
import { useMetadataMigration } from '../../MetadataAPI';
import { hasuraToast } from '../../../new-components/Toasts';
import { useCallback } from 'react';

interface UseDropSourceProps {
  customOnSuccess?: () => void;
  customOnError?: (err: Error) => void;
}

export const useDropSource = (props?: UseDropSourceProps) => {
  const invalidateMetadata = useInvalidateMetadata();
  const { mutate, ...rest } = useMetadataMigration({
    onSuccess: () => {
      hasuraToast({ type: 'success', title: 'Source dropped from metadata!' });
      invalidateMetadata();
    },
    onError: () => {
      hasuraToast({ type: 'error', title: 'Failed to drop source.' });
    },
  });

  const dropSource = useCallback(
    async (driver: string, dataSourceName: string) => {
      mutate({
        query: {
          type: `${driver}_drop_source`,
          args: {
            name: dataSourceName,
          },
        },
      });
    },
    [mutate]
  );
  return { dropSource, ...rest };
};
