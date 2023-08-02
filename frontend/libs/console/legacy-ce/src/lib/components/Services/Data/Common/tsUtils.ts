import { useCallback, useLayoutEffect, useRef } from 'react';

export function useIsUnmounted() {
  const rIsUnmounted = useRef<'mounting' | 'mounted' | 'unmounted'>('mounting');

  useLayoutEffect(() => {
    rIsUnmounted.current = 'mounted';
    return () => {
      rIsUnmounted.current = 'unmounted';
    };
  }, []);

  return useCallback(() => rIsUnmounted.current !== 'mounted', []);
}

/**
 * Type to preserve Object.entries key type:
 * https://stackoverflow.com/questions/60141960/typescript-key-value-relation-preserving-object-entries-type/60142095#60142095
 */
type Entries<T> = {
  [K in keyof T]: [K, T[K]];
}[keyof T][];

export const getEntries = <T extends Record<string, unknown>>(obj: T) =>
  Object.entries(obj) as Entries<T>;

export const isNotNull = <T>(arg: T): arg is Exclude<T, null> => {
  return arg !== null;
};

// see: https://stackoverflow.com/questions/57103834/typescript-omit-a-property-from-all-interfaces-in-a-union-but-keep-the-union-s
// TS docs: https://www.typescriptlang.org/docs/handbook/2/conditional-types.html#distributive-conditional-types
export type DistributiveOmit<T, K extends PropertyKey> = T extends any
  ? Omit<T, K>
  : never;
