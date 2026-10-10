import { useMetadata } from '@hasura/metadata/api';

export const useQueryCollections = () => {
  const { data, ...rest } = useMetadata(
    (m) => m.metadata?.query_collections ?? [],
  );

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
