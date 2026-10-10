import { LS_KEYS } from '@hasura/shared/types';

/*
In Hasura cloud, console local storage functions have been monkey-patched to separate out local storage keys
based on project ids. i.e. Hasura cloud dashboard and the different project consoles and do not share the same local
storage keys.

See monkeypatch code: https://github.com/hasura/lux/blob/0845a55/services/cloud/team_console/index.html#L24-L103

To share local storage keys between different project consoles or between cloud dashboard and console, the
localstorage key needs to be added to the `GLOBAL_LS_KEYS` variable in the above file.
*/

/* IMPORTANT: for behaviour on Cloud console, see note at start of file */
export const setLSItem = (key: string, data: string) => {
  window.localStorage.setItem(key, data);
};

/* IMPORTANT: for behaviour on Cloud console, see note at start of file */
export const getLSItem = (key: string) => {
  if (!key) {
    return null;
  }

  return window.localStorage.getItem(key);
};

/* IMPORTANT: for behaviour on Cloud console, see note at start of file */
export const removeLSItem = (key: string) => {
  const value = getLSItem(key);

  if (!value) {
    return null;
  }

  window.localStorage.removeItem(key);
  return true;
};

type expiryValue = {
  value: string;
  expiry: number;
};

export const setLSItemWithExpiry = (key: string, data: string, ttl: number) => {
  const now = new Date();

  const item: expiryValue = {
    value: data,
    expiry: now.getTime() + ttl,
  };

  setLSItem(key, JSON.stringify(item));
};

export const getItemWithExpiry = (key: string) => {
  const lsValue = getLSItem(key);
  if (!lsValue) {
    return null;
  }
  const item: expiryValue = JSON.parse(lsValue);
  const now = new Date();

  if (now.getTime() > item.expiry) {
    removeLSItem(key);
    return null;
  }

  return item.value;
};

export const getParsedLSItem = (key: string, defaultVal: any = null) => {
  const value = getLSItem(key);

  if (!value) {
    return defaultVal;
  }

  try {
    const jsonValue = JSON.parse(value);

    return jsonValue || defaultVal;
  } catch {
    return defaultVal;
  }
};

export const clearGraphiqlLS = () => {
  Object.values(LS_KEYS).forEach((lsKey) => {
    if (lsKey.startsWith('graphiql:')) {
      removeLSItem(lsKey);
    }
  });
};
