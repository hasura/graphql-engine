export const getLocalStorageItem = (key: string) => {
  return window.localStorage.getItem(key);
};

export const setLocalStorageItem = (key: string, value: string) => {
  window.localStorage.setItem(key, value);
};

/** Local storage keys **/

export const LS_VERSION_UPDATE_CHECK_LAST_CLOSED =
  'versionUpdateCheck: lastClosed';
