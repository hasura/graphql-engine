import React from 'react';
import { Choose, PathInto } from '../types';
import { ManageTableTabs } from '../features/Data/ManageTable/ManageTable';

type StringBool = 'y' | 'n';

/**
 *
 * This type defines the keys available to set/get
 *
 * Add to this type to extend available keys.
 *
 */
type SessionStorageKeys = {
  formDebug: {
    defaultOpen: {
      values: StringBool;
      errors: StringBool;
    };
    position: 'top' | 'bottom';
  };
  manageTable: {
    lastTab: ManageTableTabs;
  };
};

const getItem = <T extends PathInto<SessionStorageKeys>>(
  key: T
): Choose<SessionStorageKeys, T> | null => {
  return sessionStorage.getItem(key) as Choose<SessionStorageKeys, T> | null;
};

const setItem = <T extends PathInto<SessionStorageKeys>>(
  key: T,
  value: Choose<SessionStorageKeys, T>
) => sessionStorage.setItem(key, value);

const removeItem = (key: PathInto<SessionStorageKeys>) =>
  sessionStorage.removeItem(key);

export const sessionStore = {
  getItem,
  setItem,
  removeItem,
};

type StorageStateTuple<T extends PathInto<SessionStorageKeys>> = [
  Choose<SessionStorageKeys, T> | null,
  (nextValue: Choose<SessionStorageKeys, T>) => void
];

// a hook that mimics useState, but will get/set sessionStorage automatically
export const useSessionStoreState = <T extends PathInto<SessionStorageKeys>>(
  key: T
): StorageStateTuple<T> => {
  const [currentValue, setCurrentValue] = React.useState(getItem(key));

  const setStateAndUpdateStorage = React.useCallback(
    (nextValue: Choose<SessionStorageKeys, T>) => {
      setCurrentValue(nextValue);
      setItem(key, nextValue);
    },
    [key]
  );

  return [currentValue, setStateAndUpdateStorage];
};
