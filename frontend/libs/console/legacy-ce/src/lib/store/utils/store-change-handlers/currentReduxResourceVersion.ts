import type { OnStoreChangeProps } from '../onStoreChange';

// private module variable
let CURRENT_REDUX_RESOURCE_VERSION = 0;

/**
 * Public exported function to read private variable
 *
 * This is intended to get the current resourceVersion from redux. Will NOT react to new changes so don't use it as a reactive value
 */
export const getCurrentReduxResourceVersion = () => {
  return CURRENT_REDUX_RESOURCE_VERSION;
};

export const updateReduxResourceVersion = ({
  currentStore,
}: OnStoreChangeProps) => {
  CURRENT_REDUX_RESOURCE_VERSION = currentStore?.metadata?.resourceVersion;
};
