import React from 'react';
import { Choose, PathInto } from '@hasura/shared/types';
import { SessionStorageKeys, sessionStore } from '@hasura/shared/utils';

type StorageStateTuple<T extends PathInto<SessionStorageKeys>> = [
  Choose<SessionStorageKeys, T> | null,
  (nextValue: Choose<SessionStorageKeys, T>) => void,
];

// a hook that mimics useState, but will get/set sessionStorage automatically
export const useSessionStoreState = <T extends PathInto<SessionStorageKeys>>(
  key: T,
): StorageStateTuple<T> => {
  const [currentValue, setCurrentValue] = React.useState(
    sessionStore.getItem(key),
  );

  const setStateAndUpdateStorage = React.useCallback(
    (nextValue: Choose<SessionStorageKeys, T>) => {
      setCurrentValue(nextValue);
      sessionStore.setItem(key, nextValue);
    },
    [key],
  );

  return [currentValue, setStateAndUpdateStorage];
};
