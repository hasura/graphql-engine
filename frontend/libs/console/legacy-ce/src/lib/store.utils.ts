import { ToolkitStore } from '@reduxjs/toolkit/dist/configureStore';
import { reactQueryClient } from './lib/reactQuery';
import { Metadata } from './features/hasura-metadata-types';
import { METADATA_QUERY_KEY } from './features/hasura-metadata-api/useMetadata';

export const listenForStoreMetadataChanges = (store: ToolkitStore) => {
  let previousStore: any = null;

  store.subscribe(() => {
    if (!previousStore || previousStore?.metadata?.metadataObject == null) {
      previousStore = store.getState();
      return;
    }

    const currentStore = store.getState();

    const reactQueryResourceVersion =
      reactQueryClient.getQueryData<Metadata>(
        METADATA_QUERY_KEY
      )?.resource_version;

    if (
      currentStore?.metadata?.resourceVersion >
        previousStore?.metadata?.resourceVersion &&
      currentStore?.metadata?.resourceVersion !== reactQueryResourceVersion
    ) {
      console.groupCollapsed(
        'Metadata change occurred in redux, and is no longer in sync with react-query:'
      );

      console.info(
        `current redux store rv: ${currentStore?.metadata?.resourceVersion}`
      );
      console.info(
        `previous redux store rv: ${previousStore?.metadata?.resourceVersion}`
      );
      console.info(`react query rv: ${reactQueryResourceVersion}`);
      console.info(
        `Triggering react-query invalidation of query key: ${METADATA_QUERY_KEY}`
      );
      console.groupEnd();
      reactQueryClient.invalidateQueries(METADATA_QUERY_KEY);
    }

    previousStore = currentStore;
  });
};
