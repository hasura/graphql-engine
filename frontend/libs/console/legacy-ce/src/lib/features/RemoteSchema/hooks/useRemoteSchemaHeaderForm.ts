import { useState } from 'react';

export type HeaderItem = {
  name: string;
  type: string;
  value: string;
};

const createHeader = () => ({
  name: '',
  type: '',
  value: '',
});

const useRemoteSchemaHeaderForm = () => {
  const [headers, setHeaders] = useState([createHeader()]);

  const removeHeader = (index: number) => {
    setHeaders((prev) => [...prev.slice(0, index), ...prev.slice(index + 1)]);
  };

  const addNewHeader = () => {
    setHeaders((prev) => [...prev, createHeader()]);
  };

  const changeHeaderKey = (name: string, index: number) => {
    setHeaders((prev) =>
      prev.map((item, i) => {
        if (i === index) {
          return {
            ...item,
            name,
          };
        }

        return item;
      }),
    );
  };

  const changeHeaderValue = (value: string, index: number) => {
    setHeaders((prev) =>
      prev.map((item, i) => {
        if (i === index) {
          return {
            ...item,
            value,
          };
        }

        return item;
      }),
    );
  };

  const changeHeaderType = (headerType: string, index: number) => {
    setHeaders((prev) =>
      prev.map((item, i) => {
        if (i === index) {
          return {
            ...item,
            type: headerType,
          };
        }

        return item;
      }),
    );
  };

  return {
    headers,
    setHeaders,
    removeHeader,
    addNewHeader,
    changeHeaderKey,
    changeHeaderValue,
    changeHeaderType,
  };
};

export type UseRemoteSchemaHeaderForm = ReturnType<
  typeof useRemoteSchemaHeaderForm
>;
export default useRemoteSchemaHeaderForm;
