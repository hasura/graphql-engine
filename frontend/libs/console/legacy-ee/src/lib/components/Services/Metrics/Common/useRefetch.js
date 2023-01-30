import { useState } from 'react';

const useRefetch = defaultState => {
  const [refetchState, update] = useState(defaultState);

  const updateRefetch = ({ key, value }) => {
    update(u => {
      return {
        ...u,
        [key]: value,
      };
    });
  };
  return [refetchState, updateRefetch];
};

export default useRefetch;
