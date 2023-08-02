import { Metadata } from '../hasura-metadata-types';
import { reactQueryClient } from '../../lib/reactQuery';
import { METADATA_QUERY_KEY } from './useMetadata';

/**
 *
 * You only want to use these in helper libraries, and not in React Components.
 *
 * NEVER use these in React Components as it could produce unexpected results.
 *
 */
export const MetadataHelpers = {
  getQueryData: () =>
    reactQueryClient.getQueryData<Metadata>(METADATA_QUERY_KEY),
  invalidate: () => reactQueryClient.invalidateQueries(METADATA_QUERY_KEY),
};
