import { useEffect } from 'react';
import { hasuraToast } from '@hasura/shared/ui';

/**
 * In case of metadata loading failure, the users cannot do anything but reloading the page.
 * ATTENTION: At the time of writing, there is not a common way to handle metadata failures.
 */
export function useNotifyMetadataLoadingError(loadingMetadataFailed: boolean) {
  useEffect(() => {
    if (!loadingMetadataFailed) return;

    hasuraToast({
      title: 'Error!',
      message: 'Failed to load the metadata. Please reload the page.',
      type: 'error',
    });
  }, [loadingMetadataFailed]);
}
