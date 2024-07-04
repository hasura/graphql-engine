import { useCallback } from 'react';

import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';
import { QueryCollection } from '../../../../metadata/types';

export const removeOperationsFromQueryCollectionMetadataArgs = (
  queryCollection: string,
  queries: string[]
) => [
  ...queries.map(query => ({
    type: 'drop_query_from_collection',
    args: {
      collection_name: queryCollection,
      query_name: query,
    },
  })),
];

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

      const restEndpoints = metadata?.metadata?.rest_endpoints || [];

      const args = [];
      console.log('>>>>', queryCollection, queries, options);

      queries.forEach(endpoint => {
        if (
          restEndpoints.some(
            e =>
              e.name === endpoint.name &&
              e.definition.query.collection_name === queryCollection
          )
        ) {
          args.push({
            type: 'drop_rest_endpoint',
            args: {
              name: endpoint.name,
            },
          });
        }
      });

      args.push(
        ...removeOperationsFromQueryCollectionMetadataArgs(
          queryCollection,
          queries.map(query => query.name)
        )
      );

      return mutate(
        {
          query: {
            type: 'bulk',
            ...(metadata?.resource_version && {
              resource_version: metadata.resource_version,
            }),
            args,
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
