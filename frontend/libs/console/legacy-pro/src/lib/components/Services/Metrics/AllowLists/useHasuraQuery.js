import { useState, useEffect } from 'react';

import { makeQuery } from './Actions';

const defaultState = {
  loading: false,
  error: null,
  data: null,
};

const useHasuraQuery = ({ query, dispatcher, onError, onCompleted, run }) => {
  const [opState, updateOp] = useState(defaultState);
  const updateState = (key, val) => {
    updateOp({
      ...opState,
      [key]: val,
    });
  };
  const reset = () => updateOp({ ...defaultState });
  const fetchQuery = q => {
    updateState('loading', true);
    dispatcher(makeQuery(query || q))
      .then(data => {
        updateState('loading', false);
        updateState('data', data);
        if (onCompleted && typeof onCompleted === 'function') {
          onCompleted(data);
        }
      })
      .catch(err => {
        updateState('loading', false);
        updateState('error', err);
        if (onError && typeof onError === 'function') {
          onError(err);
        }
      });
    return () => reset();
  };

  if (typeof run === 'undefined' || run) {
    useEffect(fetchQuery, []);
  }
  return { ...opState, refetch: fetchQuery };
};

export default useHasuraQuery;
