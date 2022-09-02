import { useCallback } from 'react';

import { useMetadata, useMetadataMigration } from '@/features/MetadataAPI';
import { QueryCollection } from '@/metadata/types';

export const useAddOperationsToQueryCollection = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata } = useMetadata();

  const addOperationToQueryCollection = useCallback(
    (
      queryCollection: string,
      queries: QueryCollection[],
      options?: Parameters<typeof mutate>[1]
    ) => {
      if (!queryCollection || !queries)
        throw Error(
          `useAddOperationsToQueryCollection: Invalid input - ${
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
              type: 'add_query_to_collection',
              args: {
                collection_name: queryCollection,
                query_name: query.name,
                query: query.query,
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

  return { addOperationToQueryCollection, ...rest };
};
