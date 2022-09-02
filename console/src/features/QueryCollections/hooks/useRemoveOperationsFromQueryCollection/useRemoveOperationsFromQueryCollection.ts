import { useCallback } from 'react';

import { useMetadata, useMetadataMigration } from '@/features/MetadataAPI';
import { QueryCollection } from '@/metadata/types';

export const useRemoveOperationsFromQueryCollection = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata } = useMetadata();

  const removeOperationsFromQueryCollection = useCallback(
    (
      queryCollection: string,
      queries: QueryCollection[],
      options?: Parameters<typeof mutate>[1]
    ) => {
      if (!queryCollection || !queries)
        throw Error(
          `useRemoveOperationsFromQueryCollection: Invalid input - ${
            queryCollection && 'queryCollection'
          } ${queries && 'queries'}`
        );
      return mutate(
        {
          query: {
            type: 'bulk',
            ...(metadata?.resource_version && {
              resource_version: metadata.resource_version,
            }),
            args: queries.map(query => ({
              type: 'drop_query_from_collection',
              args: {
                collection_name: queryCollection,
                query_name: query.name,
              },
            })),
          },
        },
        {
          ...options,
        }
      );
    },
    [metadata, mutate]
  );

  return { removeOperationsFromQueryCollection, ...rest };
};
