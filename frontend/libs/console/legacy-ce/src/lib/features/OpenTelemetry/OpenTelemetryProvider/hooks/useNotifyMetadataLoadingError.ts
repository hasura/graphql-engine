import { useFireNotification } from '../../../../new-components/Notifications';
import { useEffect } from 'react';

/**
 * In case of metadata loading failure, the users cannot do anything but reloading the page.
 * ATTENTION: At the time of writing, there is not a common way to handle metadata failures.
 */
export function useNotifyMetadataLoadingError(loadingMetadataFailed: boolean) {
  const { fireNotification } = useFireNotification();

  useEffect(() => {
    if (!loadingMetadataFailed) return;

    fireNotification({
      title: 'Error!',
      message: 'Failed to load the metadata. Please reload the page.',
      type: 'error',
    });
  }, [
    // fireNotification is a stable reference
    fireNotification,
    loadingMetadataFailed,
  ]);
}
