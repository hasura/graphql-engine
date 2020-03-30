import { Header as HeaderClient, defaultHeader } from './Headers';
import { Header as HeaderServer } from '../utils/v1QueryUtils';

export const transformHeaders = (headers_?: Array<HeaderClient>) => {
  const headers = headers_ || [];
  return headers
    .map(h => {
      const transformedHeader: HeaderServer = {
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

export const addPlaceholderHeader = (newHeaders: Array<HeaderClient>) => {
  if (newHeaders.length) {
    const lastHeader = newHeaders[newHeaders.length - 1];
    if (lastHeader.name && lastHeader.value) {
      newHeaders.push(defaultHeader);
    }
  } else {
    newHeaders.push(defaultHeader);
  }
  return newHeaders;
};

export const parseServerHeaders = (headers_: Array<HeaderServer>) => {
  const headers = headers_ || [];
  return addPlaceholderHeader(
    headers.map(h => {
      const parsedHeader: HeaderClient = {
        name: h.name,
        value: h.value || "",
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
