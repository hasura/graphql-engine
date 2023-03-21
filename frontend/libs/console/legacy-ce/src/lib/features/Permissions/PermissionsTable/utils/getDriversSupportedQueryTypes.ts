import { Capabilities } from '@hasura/dc-api-types';
import isObject from 'lodash/isObject';

export const getDriversSupportedQueryTypes = (
  driverCapabilities: Capabilities
) => {
  if (!driverCapabilities) return [];

  const { mutations, queries } = driverCapabilities;
  const supportedQueryTypes = [];

  const supportedMutations =
    (isObject(mutations) &&
      Object.keys(mutations).filter(
        mutationType =>
          mutationType === 'insert' ||
          mutationType === 'update' ||
          mutationType === 'delete'
      )) ||
    [];
  supportedQueryTypes.push(...supportedMutations);

  if (queries) supportedQueryTypes.push('select');

  return supportedQueryTypes;
};
