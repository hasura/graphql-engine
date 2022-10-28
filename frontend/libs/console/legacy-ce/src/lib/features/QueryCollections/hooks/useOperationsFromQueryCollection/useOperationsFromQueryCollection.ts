import { MetadataSelector, useMetadata } from '@/features/MetadataAPI';

export const useOperationsFromQueryCollection = (queryCollectionName: string) =>
  useMetadata(
    MetadataSelector.getOperationsFromQueryCollection(queryCollectionName)
  );
