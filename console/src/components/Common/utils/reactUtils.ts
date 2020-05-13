import { useEffect, useRef } from 'react';

export const useInterval = (callback: VoidFunction, delay: number) => {
  const savedCallback: any = useRef();

  // Remember the latest callback.
  useEffect(() => {
    savedCallback.current = callback;
  }, [callback]);

  // Set up the interval.
  useEffect(() => {
    const tick = () => {
      if (savedCallback) {
        savedCallback.current();
      }
    };
    if (delay !== null) {
      const id = setInterval(tick, delay);
      return () => clearInterval(id);
    }
    return () => {};
  }, [delay]);
};

export const getReactHelmetTitle = (feature: string, service: string) => {
  return `${feature} - ${service} | Hasura`;
};
