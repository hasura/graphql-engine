import { ToolkitStore } from '@reduxjs/toolkit/dist/configureStore';
import { onStoreChange } from './onStoreChange';

// listens for any redux store changes:
export const reduxStoreListener = (store: ToolkitStore) => {
  let previousStore: any = null;

  store.subscribe(() => {
    if (!previousStore) {
      previousStore = store.getState();
      return;
    }

    const currentStore = store.getState();

    // execute code on store change:
    onStoreChange({ currentStore, previousStore });

    previousStore = currentStore;
  });
};
