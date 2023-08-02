import { useCallback } from 'react';

import {
  MetadataResponse,
  useMetadata,
  useMetadataMigration,
} from '../../../MetadataAPI';
import { QueryCollection } from '../../../../metadata/types';
import { createAllowedQueriesIfNeeded } from '../useCreateQueryCollection';

export const createAddOperationToQueryCollectionMetadataArgs = (
  queryCollection: string,
  queries: QueryCollection[],
  metadata?: MetadataResponse
) => [
  ...createAllowedQueriesIfNeeded(queryCollection, metadata),
  ...queries.map(query => ({
    type: 'add_query_to_collection',
    args: {
      collection_name: queryCollection,
      query_name: query.name,
      query: query.query,
    },
  })),
];

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
            args: createAddOperationToQueryCollectionMetadataArgs(
              queryCollection,
              queries,
              metadata
            ),
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
