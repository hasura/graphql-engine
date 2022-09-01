import { MetadataSelector, useMetadata } from '@/features/MetadataAPI';

export const useQueryCollections = () => {
  const { data, isLoading, isError, isRefetching, isSuccess } = useMetadata(
    MetadataSelector.getQueryCollections
  );

  return { data, isLoading: isLoading || isRefetching, isError, isSuccess };
};
