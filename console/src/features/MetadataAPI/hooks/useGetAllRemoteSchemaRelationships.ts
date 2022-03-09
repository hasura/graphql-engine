import { MetadataSelector } from '..';
import { useMetadata } from './useMetadata';

export const useGetAllRemoteSchemaRelationships = () => {
  return useMetadata(MetadataSelector.getAllRemoteSchemaRelationships());
};
