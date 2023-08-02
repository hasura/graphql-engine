import { MetadataSelector, useMetadata } from '../../../MetadataAPI';

export const useOperationsFromQueryCollection = (queryCollectionName: string) =>
  useMetadata(
    MetadataSelector.getOperationsFromQueryCollection(queryCollectionName)
  );
