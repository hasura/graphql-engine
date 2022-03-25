import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

export const useSources = () => {
  return useMetadata(MetadataSelector.getAllSources());
};
