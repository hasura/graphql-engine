import { useMetadataMigration, useMetadata } from '../metadata';
import type { Metadata, QueryCollectionQuery } from '@hasura/shared/types';
import { createAllowedQueriesIfNeeded } from './useCreateQueryCollection';

export const createAddOperationToQueryCollectionMetadataArgs = (
  queryCollection: string,
  queries: QueryCollectionQuery[],
  metadata?: Metadata,
) => [
  ...createAllowedQueriesIfNeeded(queryCollection, metadata),
  ...queries.map((query) => ({
    type: 'add_query_to_collection' as const,
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

  const addOperationToQueryCollection = (
    queryCollection: string,
    queries: QueryCollectionQuery[],
    options?: Parameters<typeof mutate>[1],
  ) => {
    if (!queryCollection || !queries)
      throw Error(
        `useAddOperationsToQueryCollection: Invalid input - ${
          queryCollection && 'queryCollection'
        } ${queries && 'queries'}`,
      );
    return mutate(
      {
        query: {
          type: 'bulk',
          resource_version: metadata?.resource_version,
          args: createAddOperationToQueryCollectionMetadataArgs(
            queryCollection,
            queries,
            metadata,
          ),
        },
      },
      options,
    );
  };

  return { addOperationToQueryCollection, ...rest };
};
