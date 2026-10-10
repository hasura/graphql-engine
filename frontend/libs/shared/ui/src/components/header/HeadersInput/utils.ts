import {
  HeaderConfig,
  ClientHeader,
  defaultHeader,
} from '@hasura/shared/types';
import { parseHeaderConfigs } from '@hasura/shared/utils';

export const addPlaceholderHeader = (newHeaders: ClientHeader[]) => {
  if (newHeaders.length) {
    const lastHeader = newHeaders[newHeaders.length - 1];
    if (lastHeader.name && lastHeader.value) {
      newHeaders.push({ ...defaultHeader });
    }
  } else {
    newHeaders.push({ ...defaultHeader });
  }
  return newHeaders;
};

export const parseServerHeaders = (headers: HeaderConfig[] = []) => {
  return addPlaceholderHeader(parseHeaderConfigs(headers));
};
