import { useCallback } from 'react';

import { useMetadata, useMetadataMigration } from '../../../MetadataAPI';
import { QueryCollection } from '../../../../metadata/types';

export const useEditOperationInQueryCollection = () => {
  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata } = useMetadata();

  const editOperationInQueryCollection = useCallback(
    (
      queryCollection: string,
      oldOperationName: string,
      query: QueryCollection,
      options?: Parameters<typeof mutate>[1]
    ) => {
      if (!queryCollection || !query || !oldOperationName)
        throw Error(
          `useEditOperationInQueryCollection: Invalid input - ${
            !queryCollection && 'queryCollection'
          } ${!query && ', query'} ${!oldOperationName && ', oldOperationName'}`
        );

      return mutate(
        {
          query: {
            type: 'bulk',
            ...(metadata?.resource_version && {
              resource_version: metadata.resource_version,
            }),
            args: [
              {
                type: 'drop_query_from_collection',
                args: {
                  collection_name: queryCollection,
                  query_name: oldOperationName,
                },
              },
              {
                type: 'add_query_to_collection',
                args: {
                  collection_name: queryCollection,
                  query_name: query.name,
                  query: query.query,
                },
              },
            ],
          },
        },
        {
          ...options,
        }
      );
    },
    [metadata, mutate]
  );

  return { editOperationInQueryCollection, ...rest };
};
