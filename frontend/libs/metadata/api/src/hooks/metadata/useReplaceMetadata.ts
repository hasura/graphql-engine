import type { HasuraMetadataV2, HasuraMetadataV3 } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { useMetadataMigration } from './useMetadataMigration';
import { useCallback } from 'react';
import { createTextWithLinks } from '../../utils/hyperlinkErrorMessageLink';
import { TMigrationSingleQuery } from '../../api';
import { useErrorNotification } from '../notification';

export function getReplaceMetadataQuery(
  metadata: HasuraMetadataV2 | HasuraMetadataV3,
): TMigrationSingleQuery {
  return {
    type: 'replace_metadata',
    args: metadata,
  };
}

export const useReplaceMetadata = () => {
  const mutation =
    useMetadataMigration<[{ warnings: { code: string; message: string }[] }]>();
  const showErrorNotification = useErrorNotification();

  return useCallback(
    async (
      newMetadata: HasuraMetadataV2 | HasuraMetadataV3,
      onSuccess?: () => void,
      onError?: () => void,
    ) => {
      return mutation.mutate(
        {
          query: getReplaceMetadataQuery(newMetadata),
        },
        {
          onSuccess: (response) => {
            onSuccess?.();

            hasuraToast({
              title: 'Metadata imported!',
              type: 'success',
            });

            const title = (code: string) => {
              if (code === 'illegal-event-trigger-name')
                return 'Rename Event Trigger Suggested';
              if (code === 'source-cleanup-failed') {
                return 'Manual Event Trigger Cleanup Needed';
              } else {
                return 'Time Limit Exceeded System Limit';
              }
            };

            response?.[0]?.warnings?.forEach((i) => {
              hasuraToast({
                type: 'warning',
                title: title(i.code),
                children: createTextWithLinks(i.message),
                toastOptions: {
                  duration: Infinity,
                },
              });
            });
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Failed importing metadata!',
              error: err,
            });
            onError?.();
          },
        },
      );
    },
    [mutation, showErrorNotification],
  );
};
