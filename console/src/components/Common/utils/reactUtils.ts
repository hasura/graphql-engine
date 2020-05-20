import { useEffect, useRef } from 'react';
import { Dispatch } from '../../../types';

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

/*
 * called "mapDispatchToPropsEmpty" because it just maps
 * the "dispatch" function and not any custom dispatchers
 */

export const mapDispatchToPropsEmpty = (dispatch: Dispatch) => ({ dispatch });
