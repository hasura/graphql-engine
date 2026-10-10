import { Capabilities } from '@hasura/dc-api-types';
import isObject from 'lodash/isObject';

export const getDriversSupportedQueryTypes = (
  driverCapabilities: Capabilities,
) => {
  if (!driverCapabilities) return [];

  const { mutations, queries } = driverCapabilities;
  const supportedQueryTypes: string[] = [];

  const supportedMutations: string[] =
    (isObject(mutations) &&
      Object.keys(mutations).filter(
        (mutationType) =>
          (mutationType === 'insert' ||
            mutationType === 'update' ||
            mutationType === 'delete') &&
          mutations[mutationType],
      )) ||
    [];
  supportedQueryTypes.push(...supportedMutations);

  if (queries) supportedQueryTypes.push('select');

  return supportedQueryTypes;
};
