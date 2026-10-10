import { useEffect, DependencyList } from 'react';

export const useDebouncedEffect = (
  effect: (...arg: unknown[]) => void,
  delay: number,
  deps: DependencyList,
) => {
  useEffect(() => {
    const handler = setTimeout(() => {
      effect();
    }, delay);

    return () => {
      clearTimeout(handler);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [delay, ...deps]);
};
