import { ToolkitStore } from '@reduxjs/toolkit/dist/configureStore';

let previousStore: any = null;
export const listenForStoreMetadataChanges = (store: ToolkitStore) => {
  store.subscribe(() => {
    if (!previousStore) {
      previousStore = store.getState();
      return;
    }
    const currentStore = store.getState();
    if (
      currentStore?.metadata?.resourceVersion >
      previousStore?.metadata?.resourceVersion
    ) {
      window.reactQueryClient.invalidateQueries('export_metadata');
    }

    previousStore = store.getState();
  });
};
