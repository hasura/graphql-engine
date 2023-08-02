import { useCallback } from 'react';

import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';
import { QueryCollection } from '../../../../metadata/types';
import { createAllowedQueriesIfNeeded } from '../useCreateQueryCollection';

export const useMoveOperationsToQueryCollection = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata } = useMetadata();

  const moveOperationToQueryCollection = useCallback(
    (
      fromCollection: string,
      toCollection: string,
      // considering the move action can be done on multiple queries, this hook can handle multiple queries
      queries: QueryCollection[],
      options?: Parameters<typeof mutate>[1]
    ) => {
      if (!fromCollection || !queries || !toCollection)
        throw Error(
          `useMoveOperationsToQueryCollection: Invalid input - ${
            !fromCollection && 'fromCollection'
          } ${!queries && ', queries'} ${!toCollection && ', toCollection'}`
        );

      // considering there is no direct API to edit an operation, we use bulk transaction to drop and add an operation.
      // ie. drop_query_from_collection and then recreate with add_query_to_collection in a single transaction

      return mutate(
        {
          query: {
            type: 'bulk',
            ...(metadata?.resource_version && {
              resource_version: metadata.resource_version,
            }),
            args: queries
              .map(query => [
                ...createAllowedQueriesIfNeeded(toCollection, metadata),
                {
                  type: 'drop_query_from_collection',
                  args: {
                    collection_name: fromCollection,
                    query_name: query.name,
                  },
                },
                {
                  type: 'add_query_to_collection',
                  args: {
                    collection_name: toCollection,
                    query_name: query.name,
                    query: query.query,
                  },
                },
              ])
              .flat(),
          },
        },
        {
          ...options,
        }
      );
    },
    [metadata, mutate]
  );

  return { moveOperationToQueryCollection, ...rest };
};
