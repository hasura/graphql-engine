import { useCallback, useRef } from 'react';

export function useIsFirstRender(): () => boolean {
  const isFirst = useRef(true);

  return useCallback(() => {
    if (isFirst.current) {
      isFirst.current = false;

      return true;
    }

    return isFirst.current;
  }, [isFirst.current]);
}
