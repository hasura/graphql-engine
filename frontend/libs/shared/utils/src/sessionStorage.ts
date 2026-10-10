import { Choose, PathInto } from '@hasura/shared/types';

type StringBool = 'y' | 'n';

/**
 *
 * This type defines the keys available to set/get
 *
 * Add to this type to extend available keys.
 *
 */
export type SessionStorageKeys = {
  formDebug: {
    defaultOpen: {
      values: StringBool;
      errors: StringBool;
    };
    position: 'top' | 'bottom';
  };
  manageTable: {
    lastTab: 'relationships' | 'modify' | 'browse' | 'permissions';
  };
};

const getItem = <T extends PathInto<SessionStorageKeys>>(
  key: T,
): Choose<SessionStorageKeys, T> | null => {
  return sessionStorage.getItem(key) as Choose<SessionStorageKeys, T> | null;
};

const setItem = <T extends PathInto<SessionStorageKeys>>(
  key: T,
  value: Choose<SessionStorageKeys, T>,
) => sessionStorage.setItem(key, value);

const removeItem = (key: PathInto<SessionStorageKeys>) =>
  sessionStorage.removeItem(key);

export const sessionStore = {
  getItem,
  setItem,
  removeItem,
};
