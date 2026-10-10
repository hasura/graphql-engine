import { ClientHeader, HeaderConfig } from '@hasura/shared/types';

export const transformHeaderConfigs = (
  headers: ClientHeader[] | undefined,
): HeaderConfig[] => {
  return (
    headers
      ?.filter((header) => header.value)
      .map((h) => {
        return h.type === 'value'
          ? {
              name: h.name,
              value: h.value,
            }
          : {
              name: h.name,
              value_from_env: h.value,
            };
      }) ?? []
  );
};

export const parseHeaderConfigs = (
  headers: HeaderConfig[] | undefined,
): ClientHeader[] => {
  return (
    headers?.map((h) => {
      const parsedHeader: ClientHeader =
        'value' in h
          ? {
              name: h.name,
              value: h.value || '',
              type: 'value',
            }
          : {
              name: h.name,
              value: h.value_from_env || '',
              type: 'env',
            };

      return parsedHeader;
    }) ?? []
  );
};
