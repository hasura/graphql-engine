import { useCallback, useEffect, DependencyList } from 'react';

export const useDebouncedEffect = (
  effect: (...arg: unknown[]) => void,
  delay: number,
  deps: DependencyList
) => {
  const callback = useCallback(effect, deps);

  useEffect(() => {
    const handler = setTimeout(() => {
      callback();
    }, delay);

    return () => {
      clearTimeout(handler);
    };
  }, [callback, delay]);
};
