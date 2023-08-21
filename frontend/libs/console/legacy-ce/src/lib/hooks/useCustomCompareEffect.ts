import { useRef, useEffect } from 'react';

type MaybeCleanUpFn = void | (() => void);

function useCustomCompareMemo<T>(value: T, equal: (a: T, b: T) => boolean): T {
  const ref = useRef<T>(value);

  if (!equal(value, ref.current)) {
    ref.current = value;
  }

  return ref.current;
}

function useCustomCompareEffect<T>(
  create: () => MaybeCleanUpFn,
  input: T,
  equal: (a: T, b: T) => boolean
) {
  useEffect(create, [useCustomCompareMemo(input, equal)]);
}

export default useCustomCompareEffect;
