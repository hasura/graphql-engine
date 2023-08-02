import { MetadataSelector, useMetadata } from '../../../MetadataAPI';

export const useQueryCollections = () => {
  const { data, ...rest } = useMetadata(MetadataSelector.getQueryCollections);

  return {
    data:
      data && data?.find(({ name }) => name === 'allowed-queries')
        ? data
        : [
            { name: 'allowed-queries', definition: { queries: [] } },
            ...(data || []),
          ],
    ...rest,
  };
};
