import { useMetadata } from './useMetadata';

export const useMetadataVersion = () => {
  return useMetadata(d => d.resource_version);
};
