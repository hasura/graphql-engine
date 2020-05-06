const emptyHeader = {
  name: '',
  value: '',
  type: 'static',
};

export const transformHeaders = (headers = []) => {
  return headers
    .map(h => {
      const transformedHeader = {
        name: h.name,
      };
      if (h.type === 'static') {
        transformedHeader.value = h.value;
      } else {
        transformedHeader.value_from_env = h.value;
      }
      return transformedHeader;
    })
    .filter(h => !!h.name && (!!h.value || !!h.value_from_env));
};

export const addPlaceholderHeader = newHeaders => {
  if (newHeaders.length) {
    const lastHeader = newHeaders[newHeaders.length - 1];
    if (lastHeader.name && lastHeader.value) {
      newHeaders.push(emptyHeader);
    }
  } else {
    newHeaders.push(emptyHeader);
  }
  return newHeaders;
};

export const parseServerHeaders = (headers = []) => {
  return addPlaceholderHeader(
    headers.map(h => {
      const parsedHeader = {
        name: h.name,
        value: h.value,
        type: 'static',
      };
      if (h.value_from_env) {
        parsedHeader.value = h.value_from_env;
        parsedHeader.type = 'env';
      }
      return parsedHeader;
    })
  );
};
