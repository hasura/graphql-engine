import { MetadataSelector } from '..';
import { useMetadata } from './useMetadata';

/**
 *
 * @deprecated
 * this metadta library function is no longer recommended.
 * Please use the `useMetadata` from the features/hasura-metadata-api
 */
export const useMetadataSource = (database: string) => {
  return useMetadata(MetadataSelector.getDataSourceMetadata(database));
};
