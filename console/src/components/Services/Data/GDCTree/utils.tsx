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
