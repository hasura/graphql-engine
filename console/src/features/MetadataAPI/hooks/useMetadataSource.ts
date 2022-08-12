import { MetadataSelector } from '..';
import { useMetadata } from './useMetadata';

export const useMetadataSource = (database: string) => {
  return useMetadata(MetadataSelector.getDataSourceMetadata(database));
};
